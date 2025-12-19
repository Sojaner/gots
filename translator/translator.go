package translator

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"

	"gots/lang"
)

// Translate converts the parsed program into Go source while emitting any mapping diagnostics.
func Translate(prog lang.Program) (string, []lang.Diagnostic) {
	tr := &translator{
		typeNames: map[string]string{},
		exports:   map[string]string{},
		funcSigs:  map[string][]lang.TypeRef{},
	}
	tr.indexDecls(prog)
	code := tr.renderProgram(prog)
	return code, tr.diags
}

type translator struct {
	typeNames map[string]string
	exports   map[string]string
	funcSigs  map[string][]lang.TypeRef
	diags     []lang.Diagnostic
}

func (t *translator) diag(pos lang.Position, msg string) {
	t.diags = append(t.diags, lang.Diagnostic{Message: msg, Pos: pos})
}

func (t *translator) indexDecls(prog lang.Program) {
	for _, decl := range prog.Decls {
		switch d := decl.(type) {
		case *lang.TypeDecl:
			goName := goName(d.Name, d.Exported)
			t.typeNames[d.Name] = goName
			if d.Exported {
				t.exports[d.Name] = goName
			}
		case *lang.FuncDecl:
			goName := goName(d.Name, d.Exported)
			if d.Exported {
				t.exports[d.Name] = goName
			} else {
				t.exports[d.Name] = d.Name
			}
			t.funcSigs[d.Name] = d.Results
		case *lang.VarDecl:
			goName := goName(d.Name, d.Exported)
			if d.Exported {
				t.exports[d.Name] = goName
			}
		}
	}
}

func (t *translator) renderProgram(prog lang.Program) string {
	var sb strings.Builder
	pkg := prog.Package
	if pkg == "" {
		pkg = "main"
	}
	sb.WriteString("package ")
	sb.WriteString(pkg)
	sb.WriteString("\n\n")

	if len(prog.Imports) > 0 {
		sb.WriteString("import (\n")
		for _, im := range prog.Imports {
			sb.WriteString("\t")
			if im.Alias != "" {
				sb.WriteString(im.Alias)
				sb.WriteString(" ")
			}
			sb.WriteString(strconv.Quote(im.Path))
			sb.WriteString("\n")
		}
		sb.WriteString(")\n\n")
	}

	for _, decl := range prog.Decls {
		switch d := decl.(type) {
		case *lang.TypeDecl:
			t.writeTypeDecl(&sb, d)
		}
	}

	for _, decl := range prog.Decls {
		if vd, ok := decl.(*lang.VarDecl); ok {
			t.writeVarDecl(&sb, vd)
		}
	}

	for _, decl := range prog.Decls {
		if fn, ok := decl.(*lang.FuncDecl); ok {
			t.writeFunc(&sb, fn)
		}
	}

	return sb.String()
}

func (t *translator) writeTypeDecl(sb *strings.Builder, td *lang.TypeDecl) {
	name := goName(td.Name, td.Exported)
	sb.WriteString("type ")
	sb.WriteString(name)
	if len(td.TypeParams) > 0 {
		sb.WriteString("[")
		for i, tp := range td.TypeParams {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(tp.Name)
			sb.WriteString(" ")
			if tp.Constraint != nil {
				ct, ok := t.goType(*tp.Constraint, false, nil)
				if !ok {
					ct = "any"
				}
				sb.WriteString(ct)
			} else {
				sb.WriteString("any")
			}
		}
		sb.WriteString("]")
	}
	tpSet := makeTypeParamSet(td.TypeParams)
	if td.IsInterface {
		sb.WriteString(" interface {\n")
		for _, m := range td.Methods {
			sb.WriteString("\t")
			sb.WriteString(m.Name)
			sb.WriteString("(")
			for i, p := range m.Params {
				if i > 0 {
					sb.WriteString(", ")
				}
				typ, ok := t.goType(p.Type, false, tpSet)
				if !ok {
					typ = "interface{}"
				}
				sb.WriteString(p.Name)
				sb.WriteString(" ")
				sb.WriteString(typ)
			}
			sb.WriteString(")")
			if len(m.Results) > 0 {
				res := t.renderResults(m.Results, tpSet)
				if res != "" {
					sb.WriteString(" ")
					sb.WriteString(res)
				}
			}
			sb.WriteString("\n")
		}
		sb.WriteString("}\n\n")
		return
	}

	sb.WriteString(" struct {\n")
	for _, f := range td.Fields {
		fieldName := goName(f.Name, false)
		fieldType, ok := t.goType(f.Type, false, tpSet)
		if !ok {
			fieldType = "interface{}"
		}
		sb.WriteString("\t")
		sb.WriteString(fieldName)
		sb.WriteString(" ")
		sb.WriteString(fieldType)
		sb.WriteString("\n")
	}
	sb.WriteString("}\n\n")
}

func (t *translator) writeVarDecl(sb *strings.Builder, vd *lang.VarDecl) {
	name := goName(vd.Name, vd.Exported)
	typeStr := ""
	if vd.Type != nil {
		if typ, ok := t.goType(*vd.Type, false, nil); ok {
			typeStr = typ
		}
	}
	var valStr string
	if vd.Value != nil {
		if vd.Type != nil {
			if lit, ok := t.literalForType(*vd.Type, vd.Value, nil, nil); ok {
				valStr = lit
			}
		}
		if valStr == "" {
			valStr = t.exprToString(vd.Value, nil)
		}
	}
	constMode := vd.Const
	if constMode && (valStr == "" || !isConstExpr(vd.Value)) {
		constMode = false
		t.diag(vd.Pos(), "const requires a compile-time literal; emitted as var")
	}
	if typeStr == "" && valStr == "" {
		t.diag(vd.Pos(), "declaration needs a type or an initializer")
	}
	sb.WriteString(ifIndent(0))
	if constMode {
		sb.WriteString("const ")
	} else {
		sb.WriteString("var ")
	}
	sb.WriteString(name)
	if typeStr != "" {
		sb.WriteString(" ")
		sb.WriteString(typeStr)
	}
	if valStr != "" {
		sb.WriteString(" = ")
		sb.WriteString(valStr)
	}
	sb.WriteString("\n\n")
}

func (t *translator) writeFunc(sb *strings.Builder, fn *lang.FuncDecl) {
	name := goName(fn.Name, fn.Exported)
	sb.WriteString("func ")
	if fn.Receiver != nil {
		recvName := strings.ToLower(fn.Receiver.Name)
		if recvName == "" {
			recvName = "r"
		}
		sb.WriteString("(")
		sb.WriteString(recvName)
		sb.WriteString(" ")
		rtyp, ok := t.goType(*fn.Receiver, false, nil)
		if !ok {
			rtyp = "interface{}"
		}
		sb.WriteString(rtyp)
		sb.WriteString(") ")
	}
	sb.WriteString(name)
	if len(fn.TypeParams) > 0 {
		sb.WriteString("[")
		for i, tp := range fn.TypeParams {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(tp.Name)
			sb.WriteString(" ")
			if tp.Constraint != nil {
				ct, ok := t.goType(*tp.Constraint, false, nil)
				if !ok {
					ct = "any"
				}
				sb.WriteString(ct)
			} else {
				sb.WriteString("any")
			}
		}
		sb.WriteString("]")
	}
	typeParams := makeTypeParamSet(fn.TypeParams)
	sb.WriteString("(")
	for i, p := range fn.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		typ, ok := t.goType(p.Type, false, typeParams)
		if !ok {
			typ = "interface{}"
		}
		sb.WriteString(p.Name)
		sb.WriteString(" ")
		if p.Variadic {
			sb.WriteString("...")
		}
		sb.WriteString(typ)
	}
	sb.WriteString(")")

	if len(fn.Results) > 0 {
		res := t.renderResults(fn.Results, typeParams)
		if res != "" {
			sb.WriteString(" ")
			sb.WriteString(res)
		}
	}
	sb.WriteString(" {\n")

	scope := map[string]bool{}
	for _, p := range fn.Params {
		scope[p.Name] = true
	}
	for _, stmt := range fn.Body {
		t.writeStmt(sb, stmt, 1, scope, typeParams, fn.Results)
	}
	sb.WriteString("}\n\n")
}

func (t *translator) writeStmt(sb *strings.Builder, stmt lang.Stmt, indent int, scope map[string]bool, typeParams map[string]lang.TypeParam, fnResults []lang.TypeRef) {
	switch s := stmt.(type) {
	case *lang.VarStmt:
		t.writeVarStmt(sb, s, indent, scope, typeParams, fnResults)
	case *lang.VarTupleStmt:
		t.writeVarTupleStmt(sb, s, indent, scope, typeParams, fnResults)
	case *lang.AssignStmt:
		if te, ok := s.Value.(*lang.TryExpr); ok {
			t.writeTryAssign(sb, s, te, indent, scope, typeParams, fnResults)
			return
		}
		sb.WriteString(ifIndent(indent))
		sb.WriteString(t.exprToString(s.Target, scope))
		sb.WriteString(" = ")
		sb.WriteString(t.exprToString(s.Value, scope))
		sb.WriteString("\n")
	case *lang.AssignTupleStmt:
		sb.WriteString(ifIndent(indent))
		sb.WriteString(strings.Join(s.Names, ", "))
		sb.WriteString(" = ")
		sb.WriteString(t.exprToString(s.Value, scope))
		sb.WriteString("\n")
	case *lang.ExprStmt:
		if te, ok := s.Expr.(*lang.TryExpr); ok {
			t.writeTryExprStmt(sb, te, indent, scope, typeParams, fnResults)
			return
		}
		sb.WriteString(ifIndent(indent))
		sb.WriteString(t.exprToString(s.Expr, scope))
		sb.WriteString("\n")
	case *lang.ReturnStmt:
		t.writeReturnStmt(sb, s, indent, scope, typeParams, fnResults)
	case *lang.IfStmt:
		sb.WriteString(ifIndent(indent))
		sb.WriteString("if ")
		sb.WriteString(t.exprToString(s.Cond, scope))
		sb.WriteString(" {\n")
		thenScope := cloneScope(scope)
		for _, st := range s.Then {
			t.writeStmt(sb, st, indent+1, thenScope, typeParams, fnResults)
		}
		sb.WriteString(ifIndent(indent))
		sb.WriteString("}")
		if len(s.Else) > 0 {
			sb.WriteString(" else {\n")
			elseScope := cloneScope(scope)
			for _, st := range s.Else {
				t.writeStmt(sb, st, indent+1, elseScope, typeParams, fnResults)
			}
			sb.WriteString(ifIndent(indent))
			sb.WriteString("}")
		}
		sb.WriteString("\n")
	case *lang.ForStmt:
		sb.WriteString(ifIndent(indent))
		sb.WriteString("for ")
		loopScope := cloneScope(scope)
		init := t.simpleStmtToString(s.Init, loopScope, typeParams)
		if initVar, ok := s.Init.(*lang.VarStmt); ok {
			loopScope[initVar.Name] = true
		}
		cond := "true"
		if s.Cond != nil {
			cond = t.exprToString(s.Cond, loopScope)
		}
		post := t.simpleStmtToString(s.Post, loopScope, typeParams)
		sb.WriteString(init)
		sb.WriteString("; ")
		sb.WriteString(cond)
		sb.WriteString("; ")
		sb.WriteString(post)
		sb.WriteString(" {\n")
		bodyScope := cloneScope(loopScope)
		for _, st := range s.Body {
			t.writeStmt(sb, st, indent+1, bodyScope, typeParams, fnResults)
		}
		sb.WriteString(ifIndent(indent))
		sb.WriteString("}\n")
	case *lang.RangeStmt:
		sb.WriteString(ifIndent(indent))
		loopScope := cloneScope(scope)
		key := "_"
		val := "_"
		if s.Key != "" {
			key = s.Key
			loopScope[key] = true
		}
		if s.Value != "" {
			val = s.Value
			loopScope[val] = true
		}
		sb.WriteString("for ")
		sb.WriteString(key)
		sb.WriteString(", ")
		sb.WriteString(val)
		sb.WriteString(" := range ")
		sb.WriteString(t.exprToString(s.Expr, scope))
		sb.WriteString(" {\n")
		bodyScope := cloneScope(loopScope)
		for _, st := range s.Body {
			t.writeStmt(sb, st, indent+1, bodyScope, typeParams, fnResults)
		}
		sb.WriteString(ifIndent(indent))
		sb.WriteString("}\n")
	case *lang.SwitchStmt:
		sb.WriteString(ifIndent(indent))
		sb.WriteString("switch ")
		sb.WriteString(t.exprToString(s.Expr, scope))
		sb.WriteString(" {\n")
		for _, c := range s.Cases {
			for i, v := range c.Values {
				if i == 0 {
					sb.WriteString(ifIndent(indent))
					sb.WriteString("\tcase ")
				} else {
					sb.WriteString(", ")
				}
				sb.WriteString(t.exprToString(v, scope))
			}
			sb.WriteString(":\n")
			caseScope := cloneScope(scope)
			caseScope["v"] = true
			for _, st := range c.Body {
				t.writeStmt(sb, st, indent+1, caseScope, typeParams, fnResults)
			}
		}
		if len(s.Default) > 0 {
			sb.WriteString(ifIndent(indent))
			sb.WriteString("\tdefault:\n")
			defScope := cloneScope(scope)
			defScope["v"] = true
			for _, st := range s.Default {
				t.writeStmt(sb, st, indent+1, defScope, typeParams, fnResults)
			}
		}
		sb.WriteString(ifIndent(indent))
		sb.WriteString("}\n")
	case *lang.TypeSwitchStmt:
		sb.WriteString(ifIndent(indent))
		sb.WriteString("switch v := ")
		sb.WriteString(t.exprToString(s.Expr, scope))
		sb.WriteString(".(type) {\n")
		for _, c := range s.Cases {
			sb.WriteString(ifIndent(indent))
			sb.WriteString("\tcase ")
			for i, ty := range c.Types {
				if i > 0 {
					sb.WriteString(", ")
				}
				gt, ok := t.goType(ty, false, nil)
				if !ok {
					gt = "interface{}"
				}
				sb.WriteString(gt)
			}
			sb.WriteString(":\n")
			caseScope := cloneScope(scope)
			for _, st := range c.Body {
				t.writeStmt(sb, st, indent+1, caseScope, typeParams, fnResults)
			}
		}
		if len(s.Default) > 0 {
			sb.WriteString(ifIndent(indent))
			sb.WriteString("\tdefault:\n")
			defScope := cloneScope(scope)
			for _, st := range s.Default {
				t.writeStmt(sb, st, indent+1, defScope, typeParams, fnResults)
			}
		}
		sb.WriteString(ifIndent(indent))
		sb.WriteString("}\n")
	case *lang.SelectStmt:
		sb.WriteString(ifIndent(indent))
		sb.WriteString("select {\n")
		for _, c := range s.Cases {
			sb.WriteString(ifIndent(indent))
			sb.WriteString("\tcase ")
			sb.WriteString(t.simpleStmtToString(c.Comm, scope, typeParams))
			sb.WriteString(":\n")
			caseScope := cloneScope(scope)
			for _, st := range c.Body {
				t.writeStmt(sb, st, indent+1, caseScope, typeParams, fnResults)
			}
		}
		if len(s.Default) > 0 {
			sb.WriteString(ifIndent(indent))
			sb.WriteString("\tdefault:\n")
			defScope := cloneScope(scope)
			for _, st := range s.Default {
				t.writeStmt(sb, st, indent+1, defScope, typeParams, fnResults)
			}
		}
		sb.WriteString(ifIndent(indent))
		sb.WriteString("}\n")
	case *lang.GoStmt:
		sb.WriteString(ifIndent(indent))
		sb.WriteString("go ")
		sb.WriteString(t.exprToString(s.Call, scope))
		sb.WriteString("\n")
	case *lang.SendStmt:
		sb.WriteString(ifIndent(indent))
		sb.WriteString(t.exprToString(s.Chan, scope))
		sb.WriteString(" <- ")
		sb.WriteString(t.exprToString(s.Value, scope))
		sb.WriteString("\n")
	case *lang.ThrowStmt:
		sb.WriteString(ifIndent(indent))
		sb.WriteString("panic(")
		if s.Value != nil {
			sb.WriteString(t.exprToString(s.Value, scope))
		} else {
			sb.WriteString("\"error\"")
		}
		sb.WriteString(")\n")
	case *lang.DeferStmt:
		sb.WriteString(ifIndent(indent))
		sb.WriteString("defer ")
		sb.WriteString(t.exprToString(s.Call, scope))
		sb.WriteString("\n")
	case *lang.BreakStmt:
		sb.WriteString(ifIndent(indent))
		sb.WriteString("break\n")
	case *lang.ContinueStmt:
		sb.WriteString(ifIndent(indent))
		sb.WriteString("continue\n")
	case *lang.FallthroughStmt:
		sb.WriteString(ifIndent(indent))
		sb.WriteString("fallthrough\n")
	case *lang.IncDecStmt:
		sb.WriteString(ifIndent(indent))
		sb.WriteString(t.exprToString(s.Target, scope))
		if s.Op == lang.PLUSPLUS {
			sb.WriteString("++\n")
		} else {
			sb.WriteString("--\n")
		}
	default:
		t.diag(stmt.Pos(), "statement not supported in translator")
	}
}

func (t *translator) simpleStmtToString(stmt lang.Stmt, scope map[string]bool, typeParams map[string]lang.TypeParam) string {
	_ = typeParams
	if stmt == nil {
		return ""
	}
	switch s := stmt.(type) {
	case *lang.AssignStmt:
		if _, ok := s.Value.(*lang.TryExpr); ok {
			t.diag(s.Pos(), "try is not supported inside for clauses")
			return ""
		}
		return fmt.Sprintf("%s = %s", t.exprToString(s.Target, scope), t.exprToString(s.Value, scope))
	case *lang.AssignTupleStmt:
		return fmt.Sprintf("%s = %s", strings.Join(s.Names, ", "), t.exprToString(s.Value, scope))
	case *lang.ExprStmt:
		if _, ok := s.Expr.(*lang.TryExpr); ok {
			t.diag(s.Pos(), "try is not supported inside for clauses")
			return ""
		}
		return t.exprToString(s.Expr, scope)
	case *lang.VarStmt:
		name := s.Name
		if s.Const {
			t.diag(s.Pos(), "const is not allowed in for clause; using short var")
		}
		if _, ok := s.Value.(*lang.TryExpr); ok {
			t.diag(s.Pos(), "try is not supported inside for clauses")
			return ""
		}
		val := ""
		if s.Value != nil {
			val = t.exprToString(s.Value, scope)
		}
		if val == "" {
			t.diag(s.Pos(), "for clause variable needs an initializer")
		}
		return fmt.Sprintf("%s := %s", name, val)
	case *lang.SendStmt:
		return fmt.Sprintf("%s <- %s", t.exprToString(s.Chan, scope), t.exprToString(s.Value, scope))
	default:
		t.diag(s.Pos(), "unsupported for-statement clause")
		return ""
	}
}

func (t *translator) writeVarStmt(sb *strings.Builder, vs *lang.VarStmt, indent int, scope map[string]bool, typeParams map[string]lang.TypeParam, fnResults []lang.TypeRef) {
	scope[vs.Name] = true
	typeStr := ""
	if vs.Type != nil {
		if typ, ok := t.goType(*vs.Type, false, typeParams); ok {
			typeStr = typ
		}
	}
	valStr := ""
	if te, ok := vs.Value.(*lang.TryExpr); ok {
		if indent == 0 {
			t.diag(vs.Pos(), "try is only valid inside functions")
			valStr = t.exprToString(te.Expr, scope)
		} else {
			if vs.Const {
				t.diag(vs.Pos(), "const with try is emitted as var")
			}
			t.writeTryWithTarget(sb, vs.Name, te, indent, scope, typeParams, fnResults)
			return
		}
	} else if vs.Value != nil {
		if vs.Type != nil {
			if lit, ok := t.literalForType(*vs.Type, vs.Value, scope, typeParams); ok {
				valStr = lit
			}
		}
		if valStr == "" {
			valStr = t.exprToString(vs.Value, scope)
		}
	}
	sb.WriteString(ifIndent(indent))
	if typeStr == "" && valStr == "" {
		t.diag(vs.Pos(), "declaration needs a type or an initializer")
	}
	constMode := vs.Const
	if constMode && (valStr == "" || !isConstExpr(vs.Value)) {
		t.diag(vs.Pos(), "const requires a compile-time literal; emitted as var")
		constMode = false
	}
	if constMode {
		sb.WriteString("const ")
		sb.WriteString(vs.Name)
		if typeStr != "" {
			sb.WriteString(" ")
			sb.WriteString(typeStr)
		}
		if valStr != "" {
			sb.WriteString(" = ")
			sb.WriteString(valStr)
		}
		sb.WriteString("\n")
		return
	}
	if indent > 0 && typeStr == "" && valStr != "" {
		sb.WriteString(vs.Name)
		sb.WriteString(" := ")
		sb.WriteString(valStr)
		sb.WriteString("\n")
		return
	}
	sb.WriteString("var ")
	sb.WriteString(vs.Name)
	if typeStr != "" {
		sb.WriteString(" ")
		sb.WriteString(typeStr)
	}
	if valStr != "" {
		sb.WriteString(" = ")
		sb.WriteString(valStr)
	}
	sb.WriteString("\n")
}

func (t *translator) writeVarTupleStmt(sb *strings.Builder, vs *lang.VarTupleStmt, indent int, scope map[string]bool, typeParams map[string]lang.TypeParam, fnResults []lang.TypeRef) {
	if indent == 0 {
		t.diag(vs.Pos(), "tuple declarations only allowed inside functions")
		return
	}
	if vs.Const {
		t.diag(vs.Pos(), "const tuple treated as var")
	}
	for _, n := range vs.Names {
		scope[n] = true
	}
	sb.WriteString(ifIndent(indent))
	sb.WriteString(strings.Join(vs.Names, ", "))
	sb.WriteString(" := ")
	sb.WriteString(t.exprToString(vs.Value, scope))
	sb.WriteString("\n")
}

func (t *translator) writeTryAssign(sb *strings.Builder, as *lang.AssignStmt, te *lang.TryExpr, indent int, scope map[string]bool, typeParams map[string]lang.TypeParam, fnResults []lang.TypeRef) {
	if indent == 0 {
		t.diag(te.Pos(), "try is only valid inside functions")
		return
	}
	valName := t.freshName("tmp", scope)
	t.writeTryWithTarget(sb, valName, te, indent, scope, typeParams, fnResults)
	sb.WriteString(ifIndent(indent))
	sb.WriteString(t.exprToString(as.Target, scope))
	sb.WriteString(" = ")
	sb.WriteString(valName)
	sb.WriteString("\n")
}

func (t *translator) writeTryExprStmt(sb *strings.Builder, te *lang.TryExpr, indent int, scope map[string]bool, typeParams map[string]lang.TypeParam, fnResults []lang.TypeRef) {
	if indent == 0 {
		t.diag(te.Pos(), "try is only valid inside functions")
		return
	}
	t.writeTryWithTarget(sb, "_", te, indent, scope, typeParams, fnResults)
}

func (t *translator) writeReturnStmt(sb *strings.Builder, rs *lang.ReturnStmt, indent int, scope map[string]bool, typeParams map[string]lang.TypeParam, fnResults []lang.TypeRef) {
	if len(rs.Values) == 1 {
		if te, ok := rs.Values[0].(*lang.TryExpr); ok {
			t.writeReturnTry(sb, te, indent, scope, typeParams, fnResults)
			return
		}
	}
	sb.WriteString(ifIndent(indent))
	sb.WriteString("return")
	if len(rs.Values) > 0 {
		parts := []string{}
		for _, v := range rs.Values {
			parts = append(parts, t.exprToString(v, scope))
		}
		sb.WriteString(" ")
		sb.WriteString(strings.Join(parts, ", "))
	}
	sb.WriteString("\n")
}

func (t *translator) writeReturnTry(sb *strings.Builder, te *lang.TryExpr, indent int, scope map[string]bool, typeParams map[string]lang.TypeParam, fnResults []lang.TypeRef) {
	if indent == 0 {
		t.diag(te.Pos(), "try is only valid inside functions")
		return
	}
	valName := t.writeTryWithTarget(sb, t.freshName("val", scope), te, indent, scope, typeParams, fnResults)
	sb.WriteString(ifIndent(indent))
	sb.WriteString("return ")
	if len(fnResults) > 0 && t.lastIsError(fnResults) {
		if len(fnResults) == 1 {
			sb.WriteString("nil")
		} else {
			sb.WriteString(valName)
			sb.WriteString(", nil")
		}
	} else {
		sb.WriteString(valName)
	}
	sb.WriteString("\n")
}

func (t *translator) writeTryWithTarget(sb *strings.Builder, target string, te *lang.TryExpr, indent int, scope map[string]bool, typeParams map[string]lang.TypeParam, fnResults []lang.TypeRef) string {
	if indent == 0 {
		t.diag(te.Pos(), "try is only valid inside functions")
		return target
	}
	t.validateTry(te)
	valName := target
	if valName == "" {
		valName = t.freshName("val", scope)
	}
	errName := t.freshName("err", scope)
	call := t.exprToString(te.Expr, scope)
	sb.WriteString(ifIndent(indent))
	sb.WriteString(fmt.Sprintf("%s, %s := %s\n", valName, errName, call))
	sb.WriteString(ifIndent(indent))
	sb.WriteString("if ")
	sb.WriteString(errName)
	sb.WriteString(" != nil {\n")
	catchScope := cloneScope(scope)
	if te.CatchVar != "" {
		catchScope[te.CatchVar] = true
		sb.WriteString(ifIndent(indent + 1))
		sb.WriteString(te.CatchVar)
		sb.WriteString(" := ")
		sb.WriteString(errName)
		sb.WriteString("\n")
	}
	if len(te.CatchBody) > 0 {
		for _, st := range te.CatchBody {
			t.writeStmt(sb, st, indent+1, catchScope, typeParams, fnResults)
		}
	} else {
		t.writeErrorReturn(sb, fnResults, typeParams, indent+1, errName, te.Pos())
	}
	sb.WriteString(ifIndent(indent))
	sb.WriteString("}\n")
	return valName
}

func (t *translator) writeErrorReturn(sb *strings.Builder, fnResults []lang.TypeRef, typeParams map[string]lang.TypeParam, indent int, errName string, pos lang.Position) {
	if len(fnResults) == 0 || !t.lastIsError(fnResults) {
		t.diag(pos, "try requires enclosing function to return error")
		sb.WriteString(ifIndent(indent))
		sb.WriteString("panic(\"error\")\n")
		return
	}
	parts := []string{}
	for i, r := range fnResults {
		typ, _ := t.goType(r, i == len(fnResults)-1, typeParams)
		if i == len(fnResults)-1 {
			parts = append(parts, errName)
			continue
		}
		parts = append(parts, t.zeroValue(r, typ))
	}
	sb.WriteString(ifIndent(indent))
	sb.WriteString("return ")
	sb.WriteString(strings.Join(parts, ", "))
	sb.WriteString("\n")
}

func (t *translator) zeroValue(ref lang.TypeRef, typ string) string {
	if typ == "" {
		return "nil"
	}
	if ref.Func != nil {
		return "nil"
	}
	switch ref.Name {
	case "number", "int":
		return "0"
	case "float", "float64":
		return "0"
	case "boolean", "bool":
		return "false"
	case "string":
		return "\"\""
	}
	if strings.HasPrefix(typ, "[]") || strings.HasPrefix(typ, "map[") || strings.HasPrefix(typ, "chan ") || strings.HasPrefix(typ, "*") || typ == "interface{}" || typ == "error" {
		return "nil"
	}
	return fmt.Sprintf("*new(%s)", typ)
}

func (t *translator) lastIsError(results []lang.TypeRef) bool {
	if len(results) == 0 {
		return false
	}
	return results[len(results)-1].Name == "error"
}

func (t *translator) freshName(prefix string, scope map[string]bool) string {
	name := prefix
	i := 0
	for scope != nil && scope[name] {
		i++
		name = fmt.Sprintf("%s%d", prefix, i)
	}
	return name
}

func (t *translator) validateTry(te *lang.TryExpr) {
	call, ok := te.Expr.(*lang.CallExpr)
	if !ok {
		t.diag(te.Pos(), "try expects a call expression that returns (T, error)")
		return
	}
	if id, ok := call.Callee.(*lang.IdentExpr); ok {
		if sig, ok := t.funcSigs[id.Name]; ok {
			if len(sig) != 2 || !t.lastIsError(sig) {
				t.diag(te.Pos(), "try target must return exactly (T, error)")
			}
		}
	}
}

func (t *translator) funcLitToString(fl *lang.FuncLit, scope map[string]bool) string {
	var sb strings.Builder
	sb.WriteString("func(")
	for i, p := range fl.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		typ, ok := t.goType(p.Type, false, nil)
		if !ok {
			typ = "interface{}"
		}
		sb.WriteString(p.Name)
		sb.WriteString(" ")
		if p.Variadic {
			sb.WriteString("...")
		}
		sb.WriteString(typ)
	}
	sb.WriteString(")")
	if len(fl.Results) > 0 {
		res := t.renderResults(fl.Results, nil)
		if res != "" {
			sb.WriteString(" ")
			sb.WriteString(res)
		}
	}
	sb.WriteString(" {\n")
	bodyScope := cloneScope(scope)
	for _, p := range fl.Params {
		bodyScope[p.Name] = true
	}
	for _, st := range fl.Body {
		t.writeStmt(&sb, st, 1, bodyScope, nil, nil)
	}
	sb.WriteString("}")
	return sb.String()
}

func (t *translator) literalForType(ref lang.TypeRef, expr lang.Expr, scope map[string]bool, typeParams map[string]lang.TypeParam) (string, bool) {
	switch v := expr.(type) {
	case *lang.ArrayLit:
		if ref.IsArray {
			elemRef := ref
			elemRef.IsArray = false
			elemType, ok := t.goType(elemRef, false, typeParams)
			if !ok {
				return "", false
			}
			parts := []string{}
			for _, el := range v.Elements {
				parts = append(parts, t.exprToString(el, scope))
			}
			return fmt.Sprintf("[]%s{%s}", elemType, strings.Join(parts, ", ")), true
		}
	case *lang.MapLit:
		if ref.Name == "map" && len(ref.TypeArgs) == 2 {
			keyType, ok := t.goType(ref.TypeArgs[0], false, typeParams)
			if !ok {
				return "", false
			}
			valType, ok := t.goType(ref.TypeArgs[1], false, typeParams)
			if !ok {
				return "", false
			}
			parts := []string{}
			for _, el := range v.Elements {
				parts = append(parts, fmt.Sprintf("%s: %s", strconv.Quote(el.Key), t.exprToString(el.Value, scope)))
			}
			return fmt.Sprintf("map[%s]%s{%s}", keyType, valType, strings.Join(parts, ", ")), true
		}
		// treat as struct literal
		typeStr, ok := t.goType(ref, false, typeParams)
		if !ok || ref.Name == "map" {
			return "", false
		}
		parts := []string{}
		for _, el := range v.Elements {
			parts = append(parts, fmt.Sprintf("%s: %s", goName(el.Key, false), t.exprToString(el.Value, scope)))
		}
		return fmt.Sprintf("%s{%s}", typeStr, strings.Join(parts, ", ")), true
	}
	return "", false
}

func (t *translator) exprToString(expr lang.Expr, scope map[string]bool) string {
	switch e := expr.(type) {
	case *lang.IdentExpr:
		return t.resolveName(e.Name, scope)
	case *lang.NumberLit:
		return e.Value
	case *lang.StringLit:
		return strconv.Quote(e.Value)
	case *lang.BoolLit:
		if e.Value {
			return "true"
		}
		return "false"
	case *lang.NilLit:
		return "nil"
	case *lang.MapLit:
		parts := []string{}
		for _, el := range e.Elements {
			parts = append(parts, fmt.Sprintf("%q: %s", el.Key, t.exprToString(el.Value, scope)))
		}
		return fmt.Sprintf("map[string]interface{}{%s}", strings.Join(parts, ", "))
	case *lang.ArrayLit:
		parts := []string{}
		for _, el := range e.Elements {
			parts = append(parts, t.exprToString(el, scope))
		}
		return fmt.Sprintf("[]interface{}{%s}", strings.Join(parts, ", "))
	case *lang.CallExpr:
		if id, ok := e.Callee.(*lang.IdentExpr); ok {
			switch id.Name {
			case "len", "cap":
				if len(e.Args) != 1 {
					t.diag(e.Pos(), id.Name+" expects exactly 1 argument")
				}
			case "close":
				if len(e.Args) != 1 {
					t.diag(e.Pos(), "close expects exactly 1 argument")
				}
			}
		}
		var args []string
		for _, a := range e.Args {
			args = append(args, t.exprToString(a, scope))
		}
		return fmt.Sprintf("%s(%s)", t.exprToString(e.Callee, scope), strings.Join(args, ", "))
	case *lang.MemberExpr:
		return fmt.Sprintf("%s.%s", t.exprToString(e.Object, scope), e.Property)
	case *lang.IndexExpr:
		return fmt.Sprintf("%s[%s]", t.exprToString(e.Object, scope), t.exprToString(e.Index, scope))
	case *lang.SliceExpr:
		low := ""
		high := ""
		if e.Low != nil {
			low = t.exprToString(e.Low, scope)
		}
		if e.High != nil {
			high = t.exprToString(e.High, scope)
		}
		return fmt.Sprintf("%s[%s:%s]", t.exprToString(e.Object, scope), low, high)
	case *lang.TryExpr:
		t.diag(expr.Pos(), "try expressions are only supported as standalone statements or initializers")
		return t.exprToString(e.Expr, scope)
	case *lang.FuncLit:
		return t.funcLitToString(e, scope)
	case *lang.CastExpr:
		typ, ok := t.goType(e.Type, false, nil)
		if !ok {
			typ = ""
		}
		if typ == "" {
			return t.exprToString(e.Expr, scope)
		}
		return fmt.Sprintf("%s(%s)", typ, t.exprToString(e.Expr, scope))
	case *lang.TypeAssertExpr:
		typ, ok := t.goType(e.Type, false, nil)
		if !ok {
			typ = "interface{}"
		}
		return fmt.Sprintf("(%s.(%s))", t.exprToString(e.Expr, scope), typ)
	case *lang.BinaryExpr:
		op := t.opString(e.Op)
		left := t.exprToString(e.Left, scope)
		right := t.exprToString(e.Right, scope)
		return fmt.Sprintf("(%s %s %s)", left, op, right)
	case *lang.UnaryExpr:
		if e.Op == lang.AWAIT {
			return fmt.Sprintf("(<-%s)", t.exprToString(e.Expr, scope))
		}
		op := t.opString(e.Op)
		return fmt.Sprintf("(%s%s)", op, t.exprToString(e.Expr, scope))
	default:
		t.diag(expr.Pos(), "expression not supported in translator")
		return "/* unsupported */"
	}
}

func (t *translator) resolveName(name string, scope map[string]bool) string {
	if scope != nil && scope[name] {
		return name
	}
	if mapped, ok := t.exports[name]; ok {
		return mapped
	}
	return name
}

func (t *translator) opString(op lang.TokenType) string {
	switch op {
	case lang.PLUS:
		return "+"
	case lang.MINUS:
		return "-"
	case lang.STAR:
		return "*"
	case lang.SLASH:
		return "/"
	case lang.PERCENT:
		return "%"
	case lang.AMPERSAND:
		return "&"
	case lang.PIPE:
		return "|"
	case lang.CARET:
		return "^"
	case lang.SHL:
		return "<<"
	case lang.SHR:
		return ">>"
	case lang.EQEQ:
		return "=="
	case lang.BANGEQ:
		return "!="
	case lang.LT:
		return "<"
	case lang.LTEQ:
		return "<="
	case lang.GT:
		return ">"
	case lang.GTEQ:
		return ">="
	case lang.ANDAND:
		return "&&"
	case lang.OROR:
		return "||"
	case lang.BANG:
		return "!"
	case lang.LARROW:
		return "<-"
	case lang.AWAIT:
		return "<-"
	default:
		return ""
	}
}

func (t *translator) goType(ref lang.TypeRef, allowVoid bool, typeParams map[string]lang.TypeParam) (string, bool) {
	if ref.Func != nil {
		var sb strings.Builder
		sb.WriteString("func(")
		for i, p := range ref.Func.Params {
			if i > 0 {
				sb.WriteString(", ")
			}
			pt, ok := t.goType(p.Type, false, typeParams)
			if !ok {
				return "", false
			}
			sb.WriteString(p.Name)
			if p.Name != "" {
				sb.WriteString(" ")
			}
			if p.Variadic {
				sb.WriteString("...")
			}
			sb.WriteString(pt)
		}
		sb.WriteString(")")
		if len(ref.Func.Results) > 0 {
			res := t.renderResults(ref.Func.Results, typeParams)
			if res != "" {
				sb.WriteString(" ")
				sb.WriteString(res)
			}
		}
		return sb.String(), true
	}
	base := ref.Name
	if typeParams != nil {
		if _, ok := typeParams[ref.Name]; ok {
			base = ref.Name
		}
	}
	if typeParams == nil || typeParams[ref.Name].Name == "" {
		switch ref.Name {
		case "number":
			base = "int"
		case "float":
			base = "float64"
		case "string":
			base = "string"
		case "boolean":
			base = "bool"
	case "any":
		base = "interface{}"
	case "void":
		if allowVoid && !ref.IsArray {
			return "", true
		}
		t.diag(ref.Pos, "void type is only valid as a function return")
		return "", false
	case "error":
		base = "error"
	case "map":
		base = "map"
	default:
		if mapped, ok := t.typeNames[ref.Name]; ok {
			base = mapped
		}
	}
	}

	if len(ref.TypeArgs) > 0 {
		args := []string{}
		for _, a := range ref.TypeArgs {
			at, ok := t.goType(a, false, typeParams)
			if !ok {
				return "", false
			}
			args = append(args, at)
		}
		if ref.Name == "chan" {
			if len(args) != 1 {
				t.diag(ref.Pos, "chan requires a single type argument")
				return "", false
			}
			base = "chan " + args[0]
			if ref.ChanDir == lang.ChanDirRecv {
				base = "<-chan " + args[0]
			}
			if ref.ChanDir == lang.ChanDirSend {
				base = "chan<- " + args[0]
			}
		} else if ref.Name == "map" {
			if len(args) != 2 {
				t.diag(ref.Pos, "map requires key and value type arguments, e.g. map<string, number>")
				return "", false
			}
			base = fmt.Sprintf("map[%s]%s", args[0], args[1])
		} else {
			base = fmt.Sprintf("%s[%s]", base, strings.Join(args, ", "))
		}
	}
	if ref.Name == "chan" && len(ref.TypeArgs) == 0 {
		t.diag(ref.Pos, "chan requires a type argument, e.g. chan<number>")
		return "", false
	}
	if ref.Name == "map" && len(ref.TypeArgs) == 0 {
		t.diag(ref.Pos, "map requires key and value type arguments, e.g. map<string, number>")
		return "", false
	}

	if ref.IsArray {
		base = "[]" + base
	}
	if ref.IsPointer {
		base = "*" + base
	}
	return base, true
}

func (t *translator) renderResults(results []lang.TypeRef, typeParams map[string]lang.TypeParam) string {
	if len(results) == 0 {
		return ""
	}
	out := []string{}
	for i, r := range results {
		allowVoid := i == len(results)-1
		typ, ok := t.goType(r, allowVoid, typeParams)
		if !ok {
			return ""
		}
		if typ == "" {
			continue
		}
		out = append(out, typ)
	}
	if len(out) == 0 {
		return ""
	}
	if len(out) == 1 {
		return out[0]
	}
	return fmt.Sprintf("(%s)", strings.Join(out, ", "))
}

func goName(name string, exported bool) string {
	if !exported || name == "" {
		return name
	}
	runes := []rune(name)
	runes[0] = unicode.ToUpper(runes[0])
	return string(runes)
}

func ifIndent(n int) string {
	return strings.Repeat("\t", n)
}

func cloneScope(scope map[string]bool) map[string]bool {
	out := map[string]bool{}
	for k, v := range scope {
		out[k] = v
	}
	return out
}

func makeTypeParamSet(params []lang.TypeParam) map[string]lang.TypeParam {
	out := map[string]lang.TypeParam{}
	for _, p := range params {
		out[p.Name] = p
	}
	return out
}

func isConstExpr(expr lang.Expr) bool {
	switch e := expr.(type) {
	case *lang.NumberLit, *lang.StringLit, *lang.BoolLit:
		return true
	case *lang.NilLit:
		return false
	case *lang.UnaryExpr:
		return isConstExpr(e.Expr)
	case *lang.BinaryExpr:
		return isConstExpr(e.Left) && isConstExpr(e.Right)
	default:
		return false
	}
}
