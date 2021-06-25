package scanner

import (
	"strconv"
	"strings"
	"unicode"

	gloxErrors "github.com/letung3105/lox/glox/errors"
	"github.com/letung3105/lox/glox/token"
)

// Scanner parses the input source and collects all the tokens that can be found
type Scanner struct {
	line     int
	start    int
	current  int
	source   []rune
	tokens   []*token.Token
	reporter gloxErrors.Reporter
}

// New creates a new Lox token scanner
func New(source []rune, reporter gloxErrors.Reporter) *Scanner {
	return &Scanner{1, 0, 0, source, make([]*token.Token, 0), reporter}
}

// Scan reads the source and collect all the tokens that were found from the
// source
func (scanner *Scanner) Scan() []*token.Token {
	if len(scanner.tokens) != 0 {
		return scanner.tokens
	}

	for scanner.hasNext() {
		scanner.start = scanner.current
		switch r := scanner.advance(); r {
		// Whitespaces
		case ' ', '\r', '\t':
		case '\n':
			scanner.line++
		// Single character tokens
		case '(':
			scanner.addToken(token.LEFT_PAREN, nil)
		case ')':
			scanner.addToken(token.RIGHT_PAREN, nil)
		case '{':
			scanner.addToken(token.LEFT_BRACE, nil)
		case '}':
			scanner.addToken(token.RIGHT_BRACE, nil)
		case ',':
			scanner.addToken(token.COMMA, nil)
		case '.':
			scanner.addToken(token.DOT, nil)
		case '-':
			scanner.addToken(token.MINUS, nil)
		case '+':
			scanner.addToken(token.PLUS, nil)
		case ';':
			scanner.addToken(token.SEMICOLON, nil)
		case '*':
			scanner.addToken(token.STAR, nil)
		// Double character tokens
		case '!':
			if scanner.match('=') {
				scanner.addToken(token.BANG_EQUAL, nil)
			} else {
				scanner.addToken(token.BANG, nil)
			}
		case '=':
			if scanner.match('=') {
				scanner.addToken(token.EQUAL_EQUAL, nil)
			} else {
				scanner.addToken(token.EQUAL, nil)
			}
		case '<':
			if scanner.match('=') {
				scanner.addToken(token.LESS_EQUAL, nil)
			} else {
				scanner.addToken(token.LESS, nil)
			}
		case '>':
			if scanner.match('=') {
				scanner.addToken(token.GREATER_EQUAL, nil)
			} else {
				scanner.addToken(token.GREATER, nil)
			}
		// Long lexemes
		case '/':
			if scanner.match('/') {
				// consume the comment, but keep the \n at the end of line so line
				// counting can work correctly
				for scanner.peek() != '\n' && scanner.hasNext() {
					scanner.advance()
				}
			} else {
				scanner.addToken(token.SLASH, nil)
			}
		// Literals
		case '"':
			scanner.scanString()
		default:
			if unicode.IsDigit(r) {
				scanner.scanNumber()
			} else if unicode.IsLetter(r) {
				scanner.scanIdentifier()
			} else {
				err := gloxErrors.NewGloxError(
					scanner.line,
					"",
					"Unexpected character.",
				)
				scanner.reporter.Report(err)
			}
		}
	}
	scanner.tokens = append(
		scanner.tokens,
		token.New(token.EOF, "", nil, scanner.current),
	)
	return scanner.tokens
}

func (scanner *Scanner) scanString() {
	// read until EOF or found a maching '"' --> our string includes \n
	for scanner.peek() != '"' && scanner.hasNext() {
		if scanner.peek() == '\n' {
			scanner.line++
		}
		scanner.advance()
	}

	if scanner.hasNext() {
		// consume '"'
		scanner.advance()
		// content between '"' pair
		literal := string(scanner.source[scanner.start+1 : scanner.current-1])
		scanner.addToken(token.STRING, literal)
	} else {
		err := gloxErrors.NewGloxError(scanner.line, "", "Unterminated string.")
		scanner.reporter.Report(err)
	}
}

func (scanner *Scanner) scanNumber() {
	// go through continuous digits
	for unicode.IsDigit(scanner.peek()) {
		scanner.advance()
	}
	// check if there's a '.' with following digits
	if scanner.peek() == '.' && unicode.IsDigit(scanner.peekNext()) {
		scanner.advance()
		// go through continuous digits
		for unicode.IsDigit(scanner.peek()) {
			scanner.advance()
		}
	}
	// conver to literal
	literal, err := strconv.ParseFloat(
		string(scanner.source[scanner.start:scanner.current]),
		64,
	)
	if err != nil {
		// NOTE: This should not happen
		panic(err)
	}
	scanner.addToken(token.NUMBER, literal)
}

func (scanner *Scanner) scanIdentifier() {
	for isAlphanumeric(scanner.peek()) {
		scanner.advance()
	}
	lexemeUpper := strings.ToUpper(string(scanner.source[scanner.start:scanner.current]))
	if _, isKeyword := token.Keywords[lexemeUpper]; isKeyword {
		scanner.addToken(token.Type(strings.ToUpper(lexemeUpper)), nil)
	} else {
		scanner.addToken(token.IDENTIFIER, nil)
	}
}

// addToken appends the lexeme from `start` to `current` as a token of the given
// type and carries the given literal
func (scanner *Scanner) addToken(typ token.Type, literal interface{}) {
	lexeme := string(scanner.source[scanner.start:scanner.current])
	tok := token.New(typ, lexeme, literal, scanner.line)
	scanner.tokens = append(scanner.tokens, tok)
}

// hasNext returns true if the scanner has not read pass the source length
func (scanner *Scanner) hasNext() bool {
	return scanner.current < len(scanner.source)
}

// advance consumes and returns the rune at the current possible
func (scanner *Scanner) advance() rune {
	r := scanner.source[scanner.current]
	scanner.current++
	return r
}

// match checks if the rune at the current possition is equal to the given rune,
// if they are equal, consumes the rune at the current position.
func (scanner *Scanner) match(expected rune) bool {
	if !scanner.hasNext() {
		return false
	}
	if scanner.source[scanner.current] != expected {
		return false
	}
	scanner.current++
	return true
}

// peek returns the rune at the current position, but does not consume it
func (scanner *Scanner) peek() rune {
	if !scanner.hasNext() {
		return '\x00'
	}
	return scanner.source[scanner.current]
}

// peek returns the rune at the next position, but does not consume it
func (scanner *Scanner) peekNext() rune {
	if scanner.current+1 >= len(scanner.source) {
		return '\x00'
	}
	return scanner.source[scanner.current+1]
}

func isAlphanumeric(r rune) bool {
	return unicode.IsLetter(r) || unicode.IsDigit(r)
}
