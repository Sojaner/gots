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
	AMPERSAND
	PIPE
	CARET
	SHL
	SHR
	BANG
	EQ
	ARROW
	EQEQ
	BANGEQ
	LT
	LTEQ
	GT
	GTEQ
	LARROW
	ELLIPSIS
	ANDAND
	OROR
	PLUSEQ
	MINUSEQ
	STAREQ
	SLASHEQ
	PERCENTEQ
	ANDEQ
	OREQ
	XOREQ
	SHLEQ
	SHREQ
	PLUSPLUS
	MINUSMINUS

	PACKAGE
	IMPORT
	FROM
	AS
	EXTENDS
	TYPE
	FUNCTION
	INTERFACE
	SEND
	SPAWN
	AWAIT
	LET
	CONST
	RETURN
	IF
	ELSE
	FOR
	GO
	SELECT
	SWITCH
	CASE
	DEFAULT
	FALLTHROUGH
	DEFER
	BREAK
	CONTINUE
	THROW
	TRY
	CATCH
	OF
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
	"as":       AS,
	"extends":  EXTENDS,
	"type":     TYPE,
	"function": FUNCTION,
	"interface": INTERFACE,
	"send":     SEND,
	"await":    AWAIT,
	"spawn":    SPAWN,
	"let":      LET,
	"const":    CONST,
	"return":   RETURN,
	"if":       IF,
	"else":     ELSE,
	"for":      FOR,
	"go":       GO,
	"select":   SELECT,
	"switch":   SWITCH,
	"case":     CASE,
	"default":  DEFAULT,
	"fallthrough": FALLTHROUGH,
	"defer":    DEFER,
	"break":    BREAK,
	"continue": CONTINUE,
	"throw":    THROW,
	"try":      TRY,
	"catch":    CATCH,
	"of":       OF,
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
