package lang

// Position tracks a 1-based line and column inside the source file.
type Position struct {
	Line   int
	Column int
}

// Diagnostic captures a parser or translator issue tied to a source position.
type Diagnostic struct {
	Message string
	Pos     Position
}

// Program is the root node for a .gots file.
type Program struct {
	Package string
	Imports []ImportDecl
	Decls   []Decl
}

type ImportDecl struct {
	Alias string
	Path  string
	Pos   Position
}

type Decl interface {
	declNode()
	Pos() Position
}

type TypeDecl struct {
	Name        string
	Fields      []Field
	Methods     []Method
	TypeParams  []string
	IsInterface bool
	Exported    bool
	pos         Position
}

func (t *TypeDecl) declNode()      {}
func (t *TypeDecl) Pos() Position  { return t.pos }

type Field struct {
	Name string
	Type TypeRef
	Pos  Position
}

type Method struct {
	Name    string
	Params  []Param
	Results []TypeRef
	Pos     Position
}

type FuncDecl struct {
	Receiver   *TypeRef
	Name       string
	Params     []Param
	Results    []TypeRef
	Body       []Stmt
	TypeParams []string
	Variadic   bool
	Exported   bool
	pos        Position
}

func (f *FuncDecl) declNode()     {}
func (f *FuncDecl) Pos() Position { return f.pos }

type Param struct {
	Name string
	Type TypeRef
	Variadic bool
	Pos  Position
}

type VarDecl struct {
	Name     string
	Type     *TypeRef
	Value    Expr
	Const    bool
	Exported bool
	pos      Position
}

func (v *VarDecl) declNode()     {}
func (v *VarDecl) Pos() Position { return v.pos }

type TypeRef struct {
	Name     string
	IsArray  bool
	TypeArgs []TypeRef
	Func     *FuncType
	Pos      Position
}

type FuncType struct {
	Params   []Param
	Results  []TypeRef
	Variadic bool
}

type Stmt interface {
	stmtNode()
	Pos() Position
}

type VarStmt struct {
	Name  string
	Type  *TypeRef
	Value Expr
	Const bool
	pos   Position
}

func (s *VarStmt) stmtNode()     {}
func (s *VarStmt) Pos() Position { return s.pos }

type AssignStmt struct {
	Target Expr
	Value  Expr
	pos    Position
}

func (s *AssignStmt) stmtNode()     {}
func (s *AssignStmt) Pos() Position { return s.pos }

type ExprStmt struct {
	Expr Expr
	pos  Position
}

func (s *ExprStmt) stmtNode()     {}
func (s *ExprStmt) Pos() Position { return s.pos }

type ReturnStmt struct {
	Values []Expr
	pos   Position
}

type ThrowStmt struct {
	Value Expr
	pos   Position
}

func (s *ReturnStmt) stmtNode()     {}
func (s *ReturnStmt) Pos() Position { return s.pos }
func (s *ThrowStmt) stmtNode()     {}
func (s *ThrowStmt) Pos() Position { return s.pos }

type DeferStmt struct {
	Call Expr
	pos  Position
}

func (s *DeferStmt) stmtNode()     {}
func (s *DeferStmt) Pos() Position { return s.pos }

type BreakStmt struct {
	pos Position
}

func (s *BreakStmt) stmtNode()     {}
func (s *BreakStmt) Pos() Position { return s.pos }

type ContinueStmt struct {
	pos Position
}

func (s *ContinueStmt) stmtNode()     {}
func (s *ContinueStmt) Pos() Position { return s.pos }

type FallthroughStmt struct {
	pos Position
}

func (s *FallthroughStmt) stmtNode()     {}
func (s *FallthroughStmt) Pos() Position { return s.pos }

type IfStmt struct {
	Cond Expr
	Then []Stmt
	Else []Stmt
	pos  Position
}

func (s *IfStmt) stmtNode()     {}
func (s *IfStmt) Pos() Position { return s.pos }

type ForStmt struct {
	Init Stmt
	Cond Expr
	Post Stmt
	Body []Stmt
	pos  Position
}

func (s *ForStmt) stmtNode()     {}
func (s *ForStmt) Pos() Position { return s.pos }

type RangeStmt struct {
	Key   string
	Value string
	Expr  Expr
	Body  []Stmt
	pos   Position
}

func (s *RangeStmt) stmtNode()     {}
func (s *RangeStmt) Pos() Position { return s.pos }

type SwitchStmt struct {
	Expr    Expr
	Cases   []CaseClause
	Default []Stmt
	pos     Position
}

type CaseClause struct {
	Values []Expr
	Body   []Stmt
	Pos    Position
}

func (s *SwitchStmt) stmtNode()     {}
func (s *SwitchStmt) Pos() Position { return s.pos }

type TypeSwitchStmt struct {
	Expr    Expr
	Cases   []TypeCaseClause
	Default []Stmt
	pos     Position
}

type TypeCaseClause struct {
	Types []TypeRef
	Body  []Stmt
	Pos   Position
}

func (s *TypeSwitchStmt) stmtNode()     {}
func (s *TypeSwitchStmt) Pos() Position { return s.pos }

type SelectStmt struct {
	Cases   []SelectCase
	Default []Stmt
	pos     Position
}

type SelectCase struct {
	Comm Stmt
	Body []Stmt
	Pos  Position
}

func (s *SelectStmt) stmtNode()     {}
func (s *SelectStmt) Pos() Position { return s.pos }

type GoStmt struct {
	Call Expr
	pos  Position
}

func (s *GoStmt) stmtNode()     {}
func (s *GoStmt) Pos() Position { return s.pos }

type SendStmt struct {
	Chan  Expr
	Value Expr
	pos   Position
}

func (s *SendStmt) stmtNode()     {}
func (s *SendStmt) Pos() Position { return s.pos }

type Expr interface {
	exprNode()
	Pos() Position
}

type IdentExpr struct {
	Name string
	pos  Position
}

func (e *IdentExpr) exprNode()   {}
func (e *IdentExpr) Pos() Position { return e.pos }

type NumberLit struct {
	Value string
	pos   Position
}

func (e *NumberLit) exprNode()   {}
func (e *NumberLit) Pos() Position { return e.pos }

type StringLit struct {
	Value string
	pos   Position
}

func (e *StringLit) exprNode()   {}
func (e *StringLit) Pos() Position { return e.pos }

type BoolLit struct {
	Value bool
	pos   Position
}

func (e *BoolLit) exprNode()   {}
func (e *BoolLit) Pos() Position { return e.pos }

type NilLit struct {
	pos Position
}

func (e *NilLit) exprNode()   {}
func (e *NilLit) Pos() Position { return e.pos }

type CallExpr struct {
	Callee Expr
	Args   []Expr
	pos    Position
}

func (e *CallExpr) exprNode()   {}
func (e *CallExpr) Pos() Position { return e.pos }

type TryExpr struct {
	Expr      Expr
	CatchVar  string
	CatchBody []Stmt
	pos       Position
}

func (e *TryExpr) exprNode()   {}
func (e *TryExpr) Pos() Position { return e.pos }

type BinaryExpr struct {
	Op    TokenType
	Left  Expr
	Right Expr
	pos   Position
}

func (e *BinaryExpr) exprNode()   {}
func (e *BinaryExpr) Pos() Position { return e.pos }

type UnaryExpr struct {
	Op   TokenType
	Expr Expr
	pos  Position
}

func (e *UnaryExpr) exprNode()   {}
func (e *UnaryExpr) Pos() Position { return e.pos }

type MemberExpr struct {
	Object   Expr
	Property string
	pos      Position
}

func (e *MemberExpr) exprNode()   {}
func (e *MemberExpr) Pos() Position { return e.pos }

type IndexExpr struct {
	Object Expr
	Index  Expr
	pos    Position
}

func (e *IndexExpr) exprNode()   {}
func (e *IndexExpr) Pos() Position { return e.pos }

type SliceExpr struct {
	Object Expr
	Low    Expr
	High   Expr
	pos    Position
}

func (e *SliceExpr) exprNode()   {}
func (e *SliceExpr) Pos() Position { return e.pos }

type FuncLit struct {
	Params   []Param
	Results  []TypeRef
	Body     []Stmt
	Variadic bool
	pos      Position
}

func (e *FuncLit) exprNode()   {}
func (e *FuncLit) Pos() Position { return e.pos }
