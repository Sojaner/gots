package lang

import (
	"fmt"
)

// Parse builds an AST for the given source while collecting diagnostics.
func Parse(source string) (Program, []Diagnostic) {
	lexer := NewLexer(source)
	tokens := make([]Token, 0, 256)
	diags := []Diagnostic{}

	for {
		tok := lexer.NextToken()
		if tok.Type == ILLEGAL {
			diags = append(diags, Diagnostic{
				Message: fmt.Sprintf("unexpected character %q", tok.Literal),
				Pos:     tok.Pos,
			})
		}
		tokens = append(tokens, tok)
		if tok.Type == EOF {
			break
		}
	}

	p := &parser{
		tokens: tokens,
		diags:  diags,
	}
	prog := p.parseProgram()
	return prog, p.diags
}

type parser struct {
	tokens []Token
	pos    int
	diags  []Diagnostic
}

func (p *parser) parseProgram() Program {
	prog := Program{Package: "main"}

	if p.match(PACKAGE) {
		name := p.expect(IDENT, "expected package name after package keyword")
		if name.Literal != "" {
			prog.Package = name.Literal
		}
		p.match(SEMICOLON)
	}

	for p.match(IMPORT) {
		prog.Imports = append(prog.Imports, p.parseImport())
	}

	for !p.isAtEnd() {
		exported := p.match(EXPORT)

		switch {
		case p.match(TYPE):
			td := p.parseTypeDecl()
			td.Exported = exported
			prog.Decls = append(prog.Decls, td)
		case p.match(FUNCTION):
			fd := p.parseFuncDecl()
			fd.Exported = exported
			prog.Decls = append(prog.Decls, fd)
		case p.match(LET, CONST):
			vd := p.parseTopVarDecl(p.previous(), exported)
			prog.Decls = append(prog.Decls, vd)
		case p.match(EOF):
			return prog
		default:
			p.errorAtCurrent("expected type, function, const, or let declaration")
			p.advance()
		}
	}

	return prog
}

func (p *parser) parseImport() ImportDecl {
	start := p.previous().Pos
	if p.check(STRING) {
		path := p.advance().Literal
		p.match(SEMICOLON)
		return ImportDecl{Path: path, Pos: start}
	}

	aliasTok := p.expect(IDENT, "expected import alias")
	alias := aliasTok.Literal
	var path string
	if p.match(FROM) {
		pathTok := p.expect(STRING, "expected quoted import path")
		path = pathTok.Literal
	} else {
		path = alias
		alias = ""
	}
	p.match(SEMICOLON)
	return ImportDecl{Alias: alias, Path: path, Pos: start}
}

func (p *parser) parseTypeDecl() *TypeDecl {
	nameTok := p.expect(IDENT, "expected type name")
	td := &TypeDecl{Name: nameTok.Literal, pos: nameTok.Pos}
	td.TypeParams = p.parseTypeParams()
	p.expect(EQ, "expected '=' after type name")

	if p.match(INTERFACE) {
		td.IsInterface = true
		p.expect(LBRACE, "expected '{' to start interface")
		for !p.check(RBRACE) && !p.isAtEnd() {
			mname := p.expect(IDENT, "expected method name")
			p.expect(LPAREN, "expected '(' after method name")
			params := []Param{}
			if !p.check(RPAREN) {
				for {
					pname := p.expect(IDENT, "expected parameter name")
					p.expect(COLON, "expected ':' after parameter name")
					ptype := p.parseTypeRef()
					params = append(params, Param{Name: pname.Literal, Type: ptype, Pos: pname.Pos})
					if !p.match(COMMA) {
						break
					}
				}
			}
			p.expect(RPAREN, "expected ')' to close parameter list")
			results := []TypeRef{}
			if p.match(COLON) {
				results = append(results, p.parseTypeRef())
			}
			td.Methods = append(td.Methods, Method{
				Name:    mname.Literal,
				Params:  params,
				Results: results,
				Pos:     mname.Pos,
			})
			p.match(COMMA, SEMICOLON)
		}
		p.expect(RBRACE, "expected '}' to close type declaration")
		p.match(SEMICOLON)
		return td
	}

	p.expect(LBRACE, "expected '{' to start struct type")
	for !p.check(RBRACE) && !p.isAtEnd() {
		fieldName := p.expect(IDENT, "expected field name")
		p.expect(COLON, "expected ':' after field name")
		fieldType := p.parseTypeRef()
		td.Fields = append(td.Fields, Field{
			Name: fieldName.Literal,
			Type: fieldType,
			Pos:  fieldName.Pos,
		})
		if p.match(COMMA, SEMICOLON) {
			// optional separator
		}
	}
	p.expect(RBRACE, "expected '}' to close type declaration")
	p.match(SEMICOLON)
	return td
}

func (p *parser) parseFuncDecl() *FuncDecl {
	nameTok := p.expect(IDENT, "expected function name")
	fd := &FuncDecl{Name: nameTok.Literal, pos: nameTok.Pos}
	fd.TypeParams = p.parseTypeParams()

	p.expect(LPAREN, "expected '(' after function name")
	if !p.check(RPAREN) {
		for {
			paramName := p.expect(IDENT, "expected parameter name")
			p.expect(COLON, "expected ':' after parameter name")
			paramType := p.parseTypeRef()
			fd.Params = append(fd.Params, Param{
				Name: paramName.Literal,
				Type: paramType,
				Pos:  paramName.Pos,
			})
			if !p.match(COMMA) {
				break
			}
		}
	}
	p.expect(RPAREN, "expected ')' to close parameter list")

	if p.match(COLON) {
		fd.Results = append(fd.Results, p.parseTypeRef())
	}

	fd.Body = p.parseBlock()
	return fd
}

func (p *parser) parseTopVarDecl(keyword Token, exported bool) *VarDecl {
	stmt := p.parseVarStmt(keyword)
	p.match(SEMICOLON)
	return &VarDecl{
		Name:     stmt.Name,
		Type:     stmt.Type,
		Value:    stmt.Value,
		Const:    stmt.Const,
		Exported: exported,
		pos:      keyword.Pos,
	}
}

func (p *parser) parseTypeRef() TypeRef {
	nameTok := p.expect(IDENT, "expected type name")
	ref := TypeRef{Name: nameTok.Literal, Pos: nameTok.Pos}
	ref.TypeArgs = p.parseTypeArgs()
	if p.match(LBRACKET) {
		p.expect(RBRACKET, "expected ']' after '[' in array type")
		ref.IsArray = true
	}
	return ref
}

func (p *parser) parseBlock() []Stmt {
	p.expect(LBRACE, "expected '{' to start block")
	stmts := []Stmt{}
	for !p.check(RBRACE) && !p.isAtEnd() {
		stmt := p.parseStmt()
		if stmt != nil {
			stmts = append(stmts, stmt)
		}
		p.match(SEMICOLON)
	}
	p.expect(RBRACE, "expected '}' to close block")
	return stmts
}

func (p *parser) parseStmt() Stmt {
	switch {
	case p.match(LET, CONST):
		return p.parseVarStmt(p.previous())
	case p.match(RETURN):
		return p.parseReturn()
	case p.match(THROW):
		return p.parseThrow()
	case p.match(IF):
		return p.parseIf()
	case p.match(FOR):
		return p.parseFor()
	case p.match(GO):
		pos := p.previous().Pos
		call := p.parseExpression()
		return &GoStmt{Call: call, pos: pos}
	default:
		return p.parseExprOrAssign()
	}
}

func (p *parser) parseVarStmt(keyword Token) *VarStmt {
	nameTok := p.expect(IDENT, "expected variable name")
	var typeRef *TypeRef
	if p.match(COLON) {
		ref := p.parseTypeRef()
		typeRef = &ref
	}
	var value Expr
	if p.match(EQ) {
		value = p.parseExpression()
	} else if keyword.Type == CONST {
		p.error(nameTok.Pos, "const declarations need an initializer")
	}
	return &VarStmt{
		Name:  nameTok.Literal,
		Type:  typeRef,
		Value: value,
		Const: keyword.Type == CONST,
		pos:   keyword.Pos,
	}
}

func (p *parser) parseReturn() *ReturnStmt {
	pos := p.previous().Pos
	var value Expr
	if !p.check(SEMICOLON) && !p.check(RBRACE) && !p.check(EOF) {
		value = p.parseExpression()
	}
	return &ReturnStmt{Value: value, pos: pos}
}

func (p *parser) parseThrow() *ThrowStmt {
	pos := p.previous().Pos
	var value Expr
	if !p.check(SEMICOLON) && !p.check(RBRACE) && !p.check(EOF) {
		value = p.parseExpression()
	}
	return &ThrowStmt{Value: value, pos: pos}
}

func (p *parser) parseIf() *IfStmt {
	pos := p.previous().Pos
	p.expect(LPAREN, "expected '(' after if")
	cond := p.parseExpression()
	p.expect(RPAREN, "expected ')' after condition")
	thenBlock := p.parseBlock()
	var elseBlock []Stmt
	if p.match(ELSE) {
		if p.check(IF) {
			// else-if chain
			elseIf := p.parseStmt()
			elseBlock = []Stmt{elseIf}
		} else {
			elseBlock = p.parseBlock()
		}
	}
	return &IfStmt{Cond: cond, Then: thenBlock, Else: elseBlock, pos: pos}
}

func (p *parser) parseFor() *ForStmt {
	pos := p.previous().Pos
	p.expect(LPAREN, "expected '(' after for")

	var init Stmt
	if !p.check(SEMICOLON) {
		if p.check(LET) || p.check(CONST) {
			kw := p.advance()
			init = p.parseVarStmt(kw)
		} else {
			init = p.parseExprOrAssign()
		}
	}
	p.match(SEMICOLON)

	var cond Expr
	if !p.check(SEMICOLON) {
		cond = p.parseExpression()
	}
	p.match(SEMICOLON)

	var post Stmt
	if !p.check(RPAREN) {
		post = p.parseExprOrAssign()
	}
	p.expect(RPAREN, "expected ')' after for clauses")

	body := p.parseBlock()
	return &ForStmt{Init: init, Cond: cond, Post: post, Body: body, pos: pos}
}

func (p *parser) parseExprOrAssign() Stmt {
	expr := p.parseExpression()
	if p.match(EQ) {
		value := p.parseExpression()
		return &AssignStmt{
			Target: expr,
			Value:  value,
			pos:    expr.Pos(),
		}
	}
	if p.match(LARROW) {
		value := p.parseExpression()
		return &SendStmt{
			Chan:  expr,
			Value: value,
			pos:   expr.Pos(),
		}
	}
	return &ExprStmt{Expr: expr, pos: expr.Pos()}
}

func (p *parser) parseExpression() Expr {
	return p.parseLogicalOr()
}

func (p *parser) parseTypeParams() []string {
	if !p.match(LT) {
		return nil
	}
	var params []string
	for {
		nameTok := p.expect(IDENT, "expected type parameter name")
		params = append(params, nameTok.Literal)
		if p.match(COMMA) {
			continue
		}
		break
	}
	p.expect(GT, "expected '>' after type parameters")
	return params
}

func (p *parser) parseTypeArgs() []TypeRef {
	if !p.match(LT) {
		return nil
	}
	var args []TypeRef
	for {
		args = append(args, p.parseTypeRef())
		if p.match(COMMA) {
			continue
		}
		break
	}
	p.expect(GT, "expected '>' after type arguments")
	return args
}

func (p *parser) parseLogicalOr() Expr {
	expr := p.parseLogicalAnd()
	for p.match(OROR) {
		op := p.previous()
		right := p.parseLogicalAnd()
		expr = &BinaryExpr{Op: op.Type, Left: expr, Right: right, pos: op.Pos}
	}
	return expr
}

func (p *parser) parseLogicalAnd() Expr {
	expr := p.parseEquality()
	for p.match(ANDAND) {
		op := p.previous()
		right := p.parseEquality()
		expr = &BinaryExpr{Op: op.Type, Left: expr, Right: right, pos: op.Pos}
	}
	return expr
}

func (p *parser) parseEquality() Expr {
	expr := p.parseComparison()
	for p.match(EQEQ, BANGEQ) {
		op := p.previous()
		right := p.parseComparison()
		expr = &BinaryExpr{Op: op.Type, Left: expr, Right: right, pos: op.Pos}
	}
	return expr
}

func (p *parser) parseComparison() Expr {
	expr := p.parseTerm()
	for p.match(LT, LTEQ, GT, GTEQ) {
		op := p.previous()
		right := p.parseTerm()
		expr = &BinaryExpr{Op: op.Type, Left: expr, Right: right, pos: op.Pos}
	}
	return expr
}

func (p *parser) parseTerm() Expr {
	expr := p.parseFactor()
	for p.match(PLUS, MINUS) {
		op := p.previous()
		right := p.parseFactor()
		expr = &BinaryExpr{Op: op.Type, Left: expr, Right: right, pos: op.Pos}
	}
	return expr
}

func (p *parser) parseFactor() Expr {
	expr := p.parseUnary()
	for p.match(STAR, SLASH, PERCENT) {
		op := p.previous()
		right := p.parseUnary()
		expr = &BinaryExpr{Op: op.Type, Left: expr, Right: right, pos: op.Pos}
	}
	return expr
}

func (p *parser) parseUnary() Expr {
	if p.match(BANG, MINUS, LARROW) {
		op := p.previous()
		right := p.parseUnary()
		return &UnaryExpr{Op: op.Type, Expr: right, pos: op.Pos}
	}
	return p.parsePrimary()
}

func (p *parser) parsePrimary() Expr {
	switch {
	case p.match(NUMBER):
		tok := p.previous()
		return &NumberLit{Value: tok.Literal, pos: tok.Pos}
	case p.match(STRING):
		tok := p.previous()
		return &StringLit{Value: tok.Literal, pos: tok.Pos}
	case p.match(TRUE):
		tok := p.previous()
		return &BoolLit{Value: true, pos: tok.Pos}
	case p.match(FALSE):
		tok := p.previous()
		return &BoolLit{Value: false, pos: tok.Pos}
	case p.match(NIL):
		tok := p.previous()
		return &NilLit{pos: tok.Pos}
	case p.match(IDENT):
		expr := Expr(&IdentExpr{Name: p.previous().Literal, pos: p.previous().Pos})
		return p.finishPostfix(expr)
	case p.match(LPAREN):
		expr := p.parseExpression()
		p.expect(RPAREN, "expected ')' after expression")
		return p.finishPostfix(expr)
	default:
		tok := p.peek()
		p.error(tok.Pos, "unexpected token in expression")
		p.advance()
		return &IdentExpr{Name: "", pos: tok.Pos}
	}
}

func (p *parser) finishPostfix(expr Expr) Expr {
	for {
		if p.match(LPAREN) {
			args := []Expr{}
			if !p.check(RPAREN) {
				for {
					args = append(args, p.parseExpression())
					if !p.match(COMMA) {
						break
					}
				}
			}
			paren := p.expect(RPAREN, "expected ')' after call arguments")
			expr = &CallExpr{Callee: expr, Args: args, pos: paren.Pos}
		} else if p.match(DOT) {
			prop := p.expect(IDENT, "expected property name after '.'")
			expr = &MemberExpr{Object: expr, Property: prop.Literal, pos: prop.Pos}
		} else {
			break
		}
	}
	return expr
}

func (p *parser) peek() Token {
	return p.tokens[p.pos]
}

func (p *parser) previous() Token {
	if p.pos == 0 {
		return Token{}
	}
	return p.tokens[p.pos-1]
}

func (p *parser) advance() Token {
	if !p.isAtEnd() {
		p.pos++
	}
	return p.previous()
}

func (p *parser) check(t TokenType) bool {
	if p.isAtEnd() {
		return false
	}
	return p.peek().Type == t
}

func (p *parser) match(types ...TokenType) bool {
	for _, t := range types {
		if p.check(t) {
			p.advance()
			return true
		}
	}
	return false
}

func (p *parser) expect(t TokenType, msg string) Token {
	if p.check(t) {
		return p.advance()
	}
	p.errorAtCurrent(msg)
	return Token{Type: t, Pos: p.peek().Pos}
}

func (p *parser) error(pos Position, msg string) {
	p.diags = append(p.diags, Diagnostic{Message: msg, Pos: pos})
}

func (p *parser) errorAtCurrent(msg string) {
	p.error(p.peek().Pos, msg)
}

func (p *parser) isAtEnd() bool {
	return p.peek().Type == EOF
}
