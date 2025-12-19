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
	firstTok := p.expect(IDENT, "expected function name")
	fd := &FuncDecl{pos: firstTok.Pos}
	if p.match(DOT) {
		recvType := TypeRef{Name: firstTok.Literal, Pos: firstTok.Pos}
		fd.Receiver = &recvType
		nameTok := p.expect(IDENT, "expected method name after '.'")
		fd.Name = nameTok.Literal
		fd.pos = nameTok.Pos
	} else {
		fd.Name = firstTok.Literal
	}
	fd.TypeParams = p.parseTypeParams()

	p.expect(LPAREN, "expected '(' after function name")
	if !p.check(RPAREN) {
		for {
			paramName := p.expect(IDENT, "expected parameter name")
			p.expect(COLON, "expected ':' after parameter name")
			var variadic bool
			if p.match(ELLIPSIS) {
				variadic = true
			}
			paramType := p.parseTypeRef()
			fd.Params = append(fd.Params, Param{
				Name:     paramName.Literal,
				Type:     paramType,
				Pos:      paramName.Pos,
				Variadic: variadic,
			})
			if !p.match(COMMA) {
				break
			}
		}
	}
	p.expect(RPAREN, "expected ')' to close parameter list")

	if p.match(COLON) {
		fd.Results = p.parseReturnTypes()
	}
	if len(fd.Params) > 0 && fd.Params[len(fd.Params)-1].Variadic {
		fd.Variadic = true
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
	if p.match(LPAREN) {
		return p.parseFuncType()
	}
	nameTok := p.expect(IDENT, "expected type name")
	ref := TypeRef{Name: nameTok.Literal, Pos: nameTok.Pos}
	if ref.Name == "chan" {
		return p.parseChanTypeRef(ref)
	}
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
		if p.check(IDENT) && p.pos+1 < len(p.tokens) && p.tokens[p.pos+1].Type == COMMA {
			return p.parseVarTuple(p.previous())
		}
		return p.parseVarStmt(p.previous())
	case p.match(RETURN):
		return p.parseReturn()
	case p.match(THROW):
		return p.parseThrow()
	case p.match(DEFER):
		return p.parseDefer()
	case p.match(BREAK):
		return &BreakStmt{pos: p.previous().Pos}
	case p.match(CONTINUE):
		return &ContinueStmt{pos: p.previous().Pos}
	case p.match(FALLTHROUGH):
		return &FallthroughStmt{pos: p.previous().Pos}
	case p.match(IF):
		return p.parseIf()
	case p.match(FOR):
		return p.parseFor()
	case p.match(SELECT):
		return p.parseSelect()
	case p.match(SWITCH):
		return p.parseSwitch()
	case p.match(SEND):
		return p.parseSend()
	case p.match(GO, SPAWN):
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

func (p *parser) parseVarTuple(keyword Token) Stmt {
	first := p.expect(IDENT, "expected variable name")
	p.expect(COMMA, "expected ',' in tuple declaration")
	second := p.expect(IDENT, "expected second variable name")
	p.expect(EQ, "expected '=' in tuple declaration")
	value := p.parseExpression()
	return &VarTupleStmt{
		Names: []string{first.Literal, second.Literal},
		Value: value,
		Const: keyword.Type == CONST,
		pos:   keyword.Pos,
	}
}

func (p *parser) parseReturn() *ReturnStmt {
	pos := p.previous().Pos
	var values []Expr
	if !p.check(SEMICOLON) && !p.check(RBRACE) && !p.check(EOF) {
		values = append(values, p.parseExpression())
		for p.match(COMMA) {
			values = append(values, p.parseExpression())
		}
	}
	return &ReturnStmt{Values: values, pos: pos}
}

func (p *parser) parseThrow() *ThrowStmt {
	pos := p.previous().Pos
	var value Expr
	if !p.check(SEMICOLON) && !p.check(RBRACE) && !p.check(EOF) {
		value = p.parseExpression()
	}
	return &ThrowStmt{Value: value, pos: pos}
}

func (p *parser) parseDefer() *DeferStmt {
	pos := p.previous().Pos
	call := p.parseExpression()
	return &DeferStmt{Call: call, pos: pos}
}

func (p *parser) parseSend() *SendStmt {
	pos := p.previous().Pos
	chanExpr := p.parseExpression()
	if !(p.match(ARROW) || p.match(COMMA)) {
		p.error(pos, "send expects syntax: send channel => value")
		return &SendStmt{Chan: chanExpr, Value: &IdentExpr{Name: "", pos: pos}, pos: pos}
	}
	value := p.parseExpression()
	return &SendStmt{Chan: chanExpr, Value: value, pos: pos}
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

func (p *parser) parseFor() Stmt {
	pos := p.previous().Pos
	p.expect(LPAREN, "expected '(' after for")

	if p.lookaheadRangeFor() {
		return p.parseRangeFor(pos)
	}

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

func (p *parser) lookaheadRangeFor() bool {
	if p.isAtEnd() {
		return false
	}
	tok := p.peek()
	if tok.Type != LET && tok.Type != CONST {
		return false
	}
	if p.pos+1 >= len(p.tokens) || p.tokens[p.pos+1].Type != IDENT {
		return false
	}
	i := p.pos + 2
	if i < len(p.tokens) && p.tokens[i].Type == COMMA {
		i++
		if i >= len(p.tokens) || p.tokens[i].Type != IDENT {
			return false
		}
		i++
	}
	if i < len(p.tokens) && p.tokens[i].Type == OF {
		return true
	}
	return false
}

func (p *parser) parseRangeFor(pos Position) *RangeStmt {
	kw := p.advance() // let/const
	_ = kw
	first := p.expect(IDENT, "expected variable name in range loop")
	var second Token
	if p.match(COMMA) {
		second = p.expect(IDENT, "expected second variable name in range loop")
	}
	p.expect(OF, "expected 'of' in range loop")
	expr := p.parseExpression()
	p.expect(RPAREN, "expected ')' after range expression")
	body := p.parseBlock()
	key := ""
	value := first.Literal
	if second.Literal != "" {
		key = first.Literal
		value = second.Literal
	}
	return &RangeStmt{Key: key, Value: value, Expr: expr, Body: body, pos: pos}
}

func (p *parser) parseSelect() *SelectStmt {
	pos := p.previous().Pos
	p.expect(LBRACE, "expected '{' after select")
	stmt := &SelectStmt{pos: pos}
	for !p.check(RBRACE) && !p.isAtEnd() {
		switch {
		case p.match(CASE):
			casePos := p.previous().Pos
			var comm Stmt
			switch {
			case p.match(LET, CONST):
				comm = p.parseVarStmt(p.previous())
			case p.match(SEND):
				comm = p.parseSend()
			default:
				comm = p.parseExprOrAssign()
			}
			p.expect(COLON, "expected ':' after case")
			body := []Stmt{}
			for !p.check(CASE) && !p.check(DEFAULT) && !p.check(RBRACE) && !p.isAtEnd() {
				st := p.parseStmt()
				if st != nil {
					body = append(body, st)
				}
				p.match(SEMICOLON)
			}
			stmt.Cases = append(stmt.Cases, SelectCase{Comm: comm, Body: body, Pos: casePos})
		case p.match(DEFAULT):
			p.expect(COLON, "expected ':' after default")
			body := []Stmt{}
			for !p.check(CASE) && !p.check(RBRACE) && !p.isAtEnd() {
				st := p.parseStmt()
				if st != nil {
					body = append(body, st)
				}
				p.match(SEMICOLON)
			}
			stmt.Default = body
		default:
			p.errorAtCurrent("expected case or default in select")
			p.advance()
		}
	}
	p.expect(RBRACE, "expected '}' to close select")
	return stmt
}

func (p *parser) parseSwitch() Stmt {
	pos := p.previous().Pos
	isTypeSwitch := p.match(TYPE)
	p.expect(LPAREN, "expected '(' after switch")
	expr := p.parseExpression()
	p.expect(RPAREN, "expected ')' after switch expression")
	p.expect(LBRACE, "expected '{' to start switch")

	if isTypeSwitch {
		stmt := &TypeSwitchStmt{Expr: expr, pos: pos}
		for !p.check(RBRACE) && !p.isAtEnd() {
			switch {
			case p.match(CASE):
				casePos := p.previous().Pos
				types := []TypeRef{p.parseTypeRef()}
				for p.match(COMMA) {
					types = append(types, p.parseTypeRef())
				}
				p.expect(COLON, "expected ':' after case type")
				body := []Stmt{}
				for !p.check(CASE) && !p.check(DEFAULT) && !p.check(RBRACE) && !p.isAtEnd() {
					st := p.parseStmt()
					if st != nil {
						body = append(body, st)
					}
					p.match(SEMICOLON)
				}
				stmt.Cases = append(stmt.Cases, TypeCaseClause{Types: types, Body: body, Pos: casePos})
			case p.match(DEFAULT):
				p.expect(COLON, "expected ':' after default")
				body := []Stmt{}
				for !p.check(CASE) && !p.check(RBRACE) && !p.isAtEnd() {
					st := p.parseStmt()
					if st != nil {
						body = append(body, st)
					}
					p.match(SEMICOLON)
				}
				stmt.Default = body
			default:
				p.errorAtCurrent("expected case or default in switch")
				p.advance()
			}
		}
		p.expect(RBRACE, "expected '}' to close switch")
		return stmt
	}

	stmt := &SwitchStmt{Expr: expr, pos: pos}
	for !p.check(RBRACE) && !p.isAtEnd() {
		switch {
		case p.match(CASE):
			casePos := p.previous().Pos
			values := []Expr{p.parseExpression()}
			for p.match(COMMA) {
				values = append(values, p.parseExpression())
			}
			p.expect(COLON, "expected ':' after case value")
			body := []Stmt{}
			for !p.check(CASE) && !p.check(DEFAULT) && !p.isAtEnd() && !p.check(RBRACE) {
				st := p.parseStmt()
				if st != nil {
					body = append(body, st)
				}
				p.match(SEMICOLON)
			}
			stmt.Cases = append(stmt.Cases, CaseClause{Values: values, Body: body, Pos: casePos})
		case p.match(DEFAULT):
			p.expect(COLON, "expected ':' after default")
			body := []Stmt{}
			for !p.check(CASE) && !p.check(RBRACE) && !p.isAtEnd() {
				st := p.parseStmt()
				if st != nil {
					body = append(body, st)
				}
				p.match(SEMICOLON)
			}
			stmt.Default = body
		default:
			p.errorAtCurrent("expected case or default in switch")
			p.advance()
		}
	}
	p.expect(RBRACE, "expected '}' to close switch")
	return stmt
}

func (p *parser) parseExprOrAssign() Stmt {
	expr := p.parseExpression()
	if p.match(PLUSPLUS, MINUSMINUS) {
		return &IncDecStmt{Target: expr, Op: p.previous().Type, pos: expr.Pos()}
	}
	if p.match(EQ, PLUSEQ, MINUSEQ, STAREQ, SLASHEQ, PERCENTEQ, ANDEQ, OREQ, XOREQ, SHLEQ, SHREQ) {
		op := p.previous()
		value := p.parseExpression()
		if op.Type != EQ {
			value = &BinaryExpr{Op: tokenToOp(op.Type), Left: expr, Right: value, pos: op.Pos}
		}
		return &AssignStmt{Target: expr, Value: value, Op: op.Type, pos: expr.Pos()}
	}
	if p.match(LARROW) {
		value := p.parseExpression()
		return &SendStmt{
			Chan:  expr,
			Value: value,
			pos:   expr.Pos(),
		}
	}
	if p.match(COMMA) {
		second := p.expect(IDENT, "expected identifier in tuple assignment")
		p.expect(EQ, "expected '=' in tuple assignment")
		value := p.parseExpression()
		return &AssignTupleStmt{
			Names: []string{p.extractName(expr), second.Literal},
			Value: value,
			pos:   expr.Pos(),
		}
	}
	return &ExprStmt{Expr: expr, pos: expr.Pos()}
}

func (p *parser) extractName(expr Expr) string {
	if id, ok := expr.(*IdentExpr); ok {
		return id.Name
	}
	return ""
}

func tokenToOp(tt TokenType) TokenType {
	switch tt {
	case PLUSEQ:
		return PLUS
	case MINUSEQ:
		return MINUS
	case STAREQ:
		return STAR
	case SLASHEQ:
		return SLASH
	case PERCENTEQ:
		return PERCENT
	case ANDEQ:
		return AMPERSAND
	case OREQ:
		return PIPE
	case XOREQ:
		return CARET
	case SHLEQ:
		return SHL
	case SHREQ:
		return SHR
	default:
		return EQ
	}
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

func (p *parser) parseChanTypeRef(ref TypeRef) TypeRef {
	if !p.match(LT) {
		p.error(ref.Pos, "chan requires a type argument, e.g. chan<number>")
		return ref
	}
	if p.match(IDENT) && (p.previous().Literal == "in" || p.previous().Literal == "out") {
		dir := p.previous().Literal
		elem := p.parseTypeRef()
		ref.TypeArgs = []TypeRef{elem}
		if dir == "in" {
			ref.ChanDir = ChanDirSend
		} else {
			ref.ChanDir = ChanDirRecv
		}
	} else {
		elem := p.parseTypeRef()
		ref.TypeArgs = []TypeRef{elem}
	}
	p.expect(GT, "expected '>' after chan type argument")
	return ref
}

func (p *parser) parseFuncType() TypeRef {
	start := p.previous().Pos
	params := []Param{}
	if !p.check(RPAREN) {
		for {
			var paramName string
			var paramPos Position
			if p.check(IDENT) && p.pos+1 < len(p.tokens) && p.tokens[p.pos+1].Type == COLON {
				tok := p.advance()
				paramName = tok.Literal
				paramPos = tok.Pos
				p.expect(COLON, "expected ':' in function type parameter")
			} else if p.check(IDENT) && p.pos+1 < len(p.tokens) && p.tokens[p.pos+1].Type != COLON {
				paramPos = p.peek().Pos
			}
			var variadic bool
			if p.match(ELLIPSIS) {
				variadic = true
			}
			tref := p.parseTypeRef()
			params = append(params, Param{Name: paramName, Type: tref, Pos: paramPos, Variadic: variadic})
			if !p.match(COMMA) {
				break
			}
		}
	}
	p.expect(RPAREN, "expected ')' in function type")
	p.expect(ARROW, "expected '=>' in function type")
	var results []TypeRef
	if p.match(LPAREN) {
		if !p.check(RPAREN) {
			for {
				results = append(results, p.parseTypeRef())
				if !p.match(COMMA) {
					break
				}
			}
		}
		p.expect(RPAREN, "expected ')' after function type results")
	} else {
		results = append(results, p.parseTypeRef())
	}
	if len(params) > 0 && params[len(params)-1].Variadic {
		// mark variadic last param only
		for i := 0; i < len(params)-1; i++ {
			params[i].Variadic = false
		}
	}
	return TypeRef{
		Func: &FuncType{
			Params:   params,
			Results:  results,
			Variadic: len(params) > 0 && params[len(params)-1].Variadic,
		},
		Pos: start,
	}
}

func (p *parser) parseReturnTypes() []TypeRef {
	if p.match(LPAREN) {
		var results []TypeRef
		if !p.check(RPAREN) {
			for {
				results = append(results, p.parseTypeRef())
				if !p.match(COMMA) {
					break
				}
			}
		}
		p.expect(RPAREN, "expected ')' after return types")
		return results
	}
	return []TypeRef{p.parseTypeRef()}
}

func (p *parser) parseArrowFunction(start Position) (Expr, bool) {
	save := p.pos
	params := []Param{}
	if !p.check(RPAREN) {
		for {
			nameTok := p.expect(IDENT, "expected parameter name")
			p.expect(COLON, "expected ':' after parameter name")
			var variadic bool
			if p.match(ELLIPSIS) {
				variadic = true
			}
			ptype := p.parseTypeRef()
			params = append(params, Param{Name: nameTok.Literal, Type: ptype, Pos: nameTok.Pos, Variadic: variadic})
			if !p.match(COMMA) {
				break
			}
		}
	}
	if !p.match(RPAREN) {
		p.pos = save
		return nil, false
	}
	if !p.match(ARROW) {
		p.pos = save
		return nil, false
	}
	var body []Stmt
	if p.match(LBRACE) {
		p.pos--
		body = p.parseBlock()
	} else {
		expr := p.parseExpression()
		body = []Stmt{&ReturnStmt{Values: []Expr{expr}, pos: start}}
	}
	variadic := len(params) > 0 && params[len(params)-1].Variadic
	return &FuncLit{Params: params, Results: nil, Body: body, Variadic: variadic, pos: start}, true
}

func (p *parser) parseLogicalOr() Expr {
	expr := p.parseBitwiseOr()
	for p.match(OROR) {
		op := p.previous()
		right := p.parseBitwiseOr()
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

func (p *parser) parseBitwiseOr() Expr {
	expr := p.parseBitwiseAnd()
	for p.match(PIPE, CARET) {
		op := p.previous()
		right := p.parseBitwiseAnd()
		expr = &BinaryExpr{Op: op.Type, Left: expr, Right: right, pos: op.Pos}
	}
	return expr
}

func (p *parser) parseBitwiseAnd() Expr {
	expr := p.parseShift()
	for p.match(AMPERSAND) {
		op := p.previous()
		right := p.parseShift()
		expr = &BinaryExpr{Op: op.Type, Left: expr, Right: right, pos: op.Pos}
	}
	return expr
}

func (p *parser) parseShift() Expr {
	expr := p.parseEquality()
	for p.match(SHL, SHR) {
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
	if p.match(TRY) {
		pos := p.previous().Pos
		inner := p.parseUnary()
		var catchVar string
		var catchBody []Stmt
		if p.match(CATCH) {
			if p.match(LPAREN) {
				ident := p.expect(IDENT, "expected identifier after catch")
				catchVar = ident.Literal
				p.expect(RPAREN, "expected ')' after catch identifier")
			} else if p.check(IDENT) {
				ident := p.advance()
				catchVar = ident.Literal
			}
			catchBody = p.parseBlock()
		}
		return &TryExpr{Expr: inner, CatchVar: catchVar, CatchBody: catchBody, pos: pos}
	}
	if p.match(BANG, MINUS, LARROW, AWAIT) {
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
		if arrow, ok := p.parseArrowFunction(p.previous().Pos); ok {
			return arrow
		}
		expr := p.parseExpression()
		p.expect(RPAREN, "expected ')' after expression")
		return p.finishPostfix(expr)
	case p.match(LBRACKET):
		pos := p.previous().Pos
		elems := []Expr{}
		if !p.check(RBRACKET) {
			for {
				elems = append(elems, p.parseExpression())
				if !p.match(COMMA) {
					break
				}
			}
		}
		p.expect(RBRACKET, "expected ']' after array literal")
		return p.finishPostfix(&ArrayLit{Elements: elems, pos: pos})
	case p.match(LBRACE):
		pos := p.previous().Pos
		elems := []MapElement{}
		if !p.check(RBRACE) {
			for {
				var key string
				if p.match(STRING) {
					key = p.previous().Literal
				} else {
					tok := p.expect(IDENT, "expected key in map literal")
					key = tok.Literal
				}
				p.expect(COLON, "expected ':' after key")
				val := p.parseExpression()
				elems = append(elems, MapElement{Key: key, Value: val, Pos: pos})
				if !p.match(COMMA) {
					break
				}
			}
		}
		p.expect(RBRACE, "expected '}' after map literal")
		return &MapLit{Elements: elems, pos: pos}
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
		} else if p.match(AS) {
			ty := p.parseTypeRef()
			expr = &TypeAssertExpr{Expr: expr, Type: ty, pos: ty.Pos}
		} else if p.match(LBRACKET) {
			lpos := p.previous().Pos
			var low Expr
			if !p.check(COLON) && !p.check(RBRACKET) {
				low = p.parseExpression()
			}
			if p.match(COLON) {
				var high Expr
				if !p.check(RBRACKET) {
					high = p.parseExpression()
				}
				p.expect(RBRACKET, "expected ']' after slice")
				expr = &SliceExpr{Object: expr, Low: low, High: high, pos: lpos}
			} else {
				p.expect(RBRACKET, "expected ']' after index")
				if low == nil {
					p.error(lpos, "index expression requires a value")
					low = &IdentExpr{Name: "", pos: lpos}
				}
				expr = &IndexExpr{Object: expr, Index: low, pos: lpos}
			}
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
