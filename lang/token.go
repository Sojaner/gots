package lang

type TokenType int

const (
	ILLEGAL TokenType = iota
	EOF
	IDENT
	NUMBER
	STRING

	LPAREN
	RPAREN
	LBRACE
	RBRACE
	LBRACKET
	RBRACKET
	COMMA
	SEMICOLON
	COLON
	DOT

	PLUS
	MINUS
	STAR
	SLASH
	PERCENT
	BANG
	EQ
	EQEQ
	BANGEQ
	LT
	LTEQ
	GT
	GTEQ
	LARROW
	ANDAND
	OROR

	PACKAGE
	IMPORT
	FROM
	TYPE
	FUNCTION
	INTERFACE
	LET
	CONST
	RETURN
	IF
	ELSE
	FOR
	GO
	THROW
	EXPORT
	TRUE
	FALSE
	NIL
)

type Token struct {
	Type    TokenType
	Literal string
	Pos     Position
}

var keywords = map[string]TokenType{
	"package":  PACKAGE,
	"import":   IMPORT,
	"from":     FROM,
	"type":     TYPE,
	"function": FUNCTION,
	"interface": INTERFACE,
	"let":      LET,
	"const":    CONST,
	"return":   RETURN,
	"if":       IF,
	"else":     ELSE,
	"for":      FOR,
	"go":       GO,
	"throw":    THROW,
	"export":   EXPORT,
	"true":     TRUE,
	"false":    FALSE,
	"nil":      NIL,
}

func lookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return IDENT
}
