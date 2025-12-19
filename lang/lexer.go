package lang

import (
	"strings"
	"unicode"
)

type Lexer struct {
	input  string
	offset int
	line   int
	col    int
	ch     rune
}

func NewLexer(input string) *Lexer {
	l := &Lexer{
		input: input,
		line:  1,
		col:   0,
	}
	l.read()
	return l
}

func (l *Lexer) read() {
	if l.offset >= len(l.input) {
		l.ch = 0
		return
	}
	l.ch = rune(l.input[l.offset])
	l.offset++
	if l.ch == '\n' {
		l.line++
		l.col = 0
	} else {
		l.col++
	}
}

func (l *Lexer) peek() rune {
	if l.offset >= len(l.input) {
		return 0
	}
	return rune(l.input[l.offset])
}

func (l *Lexer) NextToken() Token {
	l.skipWhitespaceAndComments()
	pos := Position{Line: l.line, Column: l.col}

	switch l.ch {
	case 0:
		return Token{Type: EOF, Pos: pos}
	case '(':
		tok := Token{Type: LPAREN, Literal: "(", Pos: pos}
		l.read()
		return tok
	case ')':
		tok := Token{Type: RPAREN, Literal: ")", Pos: pos}
		l.read()
		return tok
	case '{':
		tok := Token{Type: LBRACE, Literal: "{", Pos: pos}
		l.read()
		return tok
	case '}':
		tok := Token{Type: RBRACE, Literal: "}", Pos: pos}
		l.read()
		return tok
	case '[':
		tok := Token{Type: LBRACKET, Literal: "[", Pos: pos}
		l.read()
		return tok
	case ']':
		tok := Token{Type: RBRACKET, Literal: "]", Pos: pos}
		l.read()
		return tok
	case ',':
		tok := Token{Type: COMMA, Literal: ",", Pos: pos}
		l.read()
		return tok
	case ';':
		tok := Token{Type: SEMICOLON, Literal: ";", Pos: pos}
		l.read()
		return tok
	case ':':
		tok := Token{Type: COLON, Literal: ":", Pos: pos}
		l.read()
		return tok
	case '.':
		if l.peek() == '.' && l.offset+1 < len(l.input) && l.input[l.offset+1] == '.' {
			l.read()
			l.read()
			tok := Token{Type: ELLIPSIS, Literal: "...", Pos: pos}
			l.read()
			return tok
		}
		tok := Token{Type: DOT, Literal: ".", Pos: pos}
		l.read()
		return tok
	case '+':
		if l.peek() == '+' {
			l.read()
			tok := Token{Type: PLUSPLUS, Literal: "++", Pos: pos}
			l.read()
			return tok
		}
		if l.peek() == '=' {
			l.read()
			tok := Token{Type: PLUSEQ, Literal: "+=", Pos: pos}
			l.read()
			return tok
		}
		tok := Token{Type: PLUS, Literal: "+", Pos: pos}
		l.read()
		return tok
	case '-':
		if l.peek() == '-' {
			l.read()
			tok := Token{Type: MINUSMINUS, Literal: "--", Pos: pos}
			l.read()
			return tok
		}
		if l.peek() == '=' {
			l.read()
			tok := Token{Type: MINUSEQ, Literal: "-=", Pos: pos}
			l.read()
			return tok
		}
		tok := Token{Type: MINUS, Literal: "-", Pos: pos}
		l.read()
		return tok
	case '*':
		if l.peek() == '=' {
			l.read()
			tok := Token{Type: STAREQ, Literal: "*=", Pos: pos}
			l.read()
			return tok
		}
		tok := Token{Type: STAR, Literal: "*", Pos: pos}
		l.read()
		return tok
	case '%':
		if l.peek() == '=' {
			l.read()
			tok := Token{Type: PERCENTEQ, Literal: "%=", Pos: pos}
			l.read()
			return tok
		}
		tok := Token{Type: PERCENT, Literal: "%", Pos: pos}
		l.read()
		return tok
	case '!':
		if l.peek() == '=' {
			l.read()
			tok := Token{Type: BANGEQ, Literal: "!=", Pos: pos}
			l.read()
			return tok
		}
		tok := Token{Type: BANG, Literal: "!", Pos: pos}
		l.read()
		return tok
	case '=':
		if l.peek() == '=' {
			l.read()
			tok := Token{Type: EQEQ, Literal: "==", Pos: pos}
			l.read()
			return tok
		}
		if l.peek() == '>' {
			l.read()
			tok := Token{Type: ARROW, Literal: "=>", Pos: pos}
			l.read()
			return tok
		}
		tok := Token{Type: EQ, Literal: "=", Pos: pos}
		l.read()
		return tok
	case '<':
		if l.peek() == '<' {
			l.read()
			if l.peek() == '=' {
				l.read()
				tok := Token{Type: SHLEQ, Literal: "<<=", Pos: pos}
				l.read()
				return tok
			}
			tok := Token{Type: SHL, Literal: "<<", Pos: pos}
			l.read()
			return tok
		}
		if l.peek() == '-' {
			l.read()
			tok := Token{Type: LARROW, Literal: "<-", Pos: pos}
			l.read()
			return tok
		}
		if l.peek() == '=' {
			l.read()
			tok := Token{Type: LTEQ, Literal: "<=", Pos: pos}
			l.read()
			return tok
		}
		tok := Token{Type: LT, Literal: "<", Pos: pos}
		l.read()
		return tok
	case '>':
		if l.peek() == '>' {
			l.read()
			if l.peek() == '=' {
				l.read()
				tok := Token{Type: SHREQ, Literal: ">>=", Pos: pos}
				l.read()
				return tok
			}
			tok := Token{Type: SHR, Literal: ">>", Pos: pos}
			l.read()
			return tok
		}
		if l.peek() == '=' {
			l.read()
			tok := Token{Type: GTEQ, Literal: ">=", Pos: pos}
			l.read()
			return tok
		}
		tok := Token{Type: GT, Literal: ">", Pos: pos}
		l.read()
		return tok
	case '&':
		if l.peek() == '&' {
			l.read()
			tok := Token{Type: ANDAND, Literal: "&&", Pos: pos}
			l.read()
			return tok
		}
		if l.peek() == '=' {
			l.read()
			tok := Token{Type: ANDEQ, Literal: "&=", Pos: pos}
			l.read()
			return tok
		}
		tok := Token{Type: AMPERSAND, Literal: "&", Pos: pos}
		l.read()
		return tok
	case '|':
		if l.peek() == '|' {
			l.read()
			tok := Token{Type: OROR, Literal: "||", Pos: pos}
			l.read()
			return tok
		}
		if l.peek() == '=' {
			l.read()
			tok := Token{Type: OREQ, Literal: "|=", Pos: pos}
			l.read()
			return tok
		}
		tok := Token{Type: PIPE, Literal: "|", Pos: pos}
		l.read()
		return tok
	case '^':
		if l.peek() == '=' {
			l.read()
			tok := Token{Type: XOREQ, Literal: "^=", Pos: pos}
			l.read()
			return tok
		}
		tok := Token{Type: CARET, Literal: "^", Pos: pos}
		l.read()
		return tok
	case '"':
		literal := l.readString()
		return Token{Type: STRING, Literal: literal, Pos: pos}
	}

	if isLetter(l.ch) {
		lit := l.readIdentifier()
		return Token{Type: lookupIdent(lit), Literal: lit, Pos: pos}
	}
	if isDigit(l.ch) {
		lit := l.readNumber()
		return Token{Type: NUMBER, Literal: lit, Pos: pos}
	}

	tok := Token{Type: ILLEGAL, Literal: string(l.ch), Pos: pos}
	l.read()
	return tok
}

func (l *Lexer) skipWhitespaceAndComments() {
	for {
		if l.ch == ' ' || l.ch == '\t' || l.ch == '\r' || l.ch == '\n' {
			l.read()
			continue
		}
		if l.ch == '/' && l.peek() == '/' {
			for l.ch != '\n' && l.ch != 0 {
				l.read()
			}
			continue
		}
		break
	}
}

func (l *Lexer) readIdentifier() string {
	var b strings.Builder
	for isLetter(l.ch) || isDigit(l.ch) {
		b.WriteRune(l.ch)
		l.read()
	}
	return b.String()
}

func (l *Lexer) readNumber() string {
	var b strings.Builder
	for isDigit(l.ch) {
		b.WriteRune(l.ch)
		l.read()
	}
	if l.ch == '.' {
		b.WriteRune(l.ch)
		l.read()
		for isDigit(l.ch) {
			b.WriteRune(l.ch)
			l.read()
		}
	}
	return b.String()
}

func (l *Lexer) readString() string {
	// Skip opening quote.
	l.read()
	var b strings.Builder
	for l.ch != '"' && l.ch != 0 {
		b.WriteRune(l.ch)
		l.read()
	}
	// Skip closing quote if present.
	if l.ch == '"' {
		l.read()
	}
	return b.String()
}

func isLetter(ch rune) bool {
	return unicode.IsLetter(ch) || ch == '_'
}

func isDigit(ch rune) bool {
	return ch >= '0' && ch <= '9'
}
