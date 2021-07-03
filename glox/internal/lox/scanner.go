package lox

import (
	"strconv"
	"unicode"
)

// Scanner parses the input source and collects all the tokens that can be found
type Scanner struct {
	line     int
	start    int
	current  int
	source   []rune
	tokens   []*loxToken
	reporter Reporter
}

// New creates a new Lox token scanner
func NewScanner(source []rune, reporter Reporter) *Scanner {
	scanner := new(Scanner)
	scanner.line = 1
	scanner.start = 0
	scanner.current = 0
	scanner.source = source
	scanner.tokens = make([]*loxToken, 0)
	scanner.reporter = reporter
	return scanner
}

// Scan reads the source and collect all the tokens that were found from the
// source
func (scanner *Scanner) Scan() []*loxToken {
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
			scanner.addToken(tokenLeftParen, nil)
		case ')':
			scanner.addToken(tokenRightParen, nil)
		case '{':
			scanner.addToken(tokenLeftBrace, nil)
		case '}':
			scanner.addToken(tokenRightBrace, nil)
		case ',':
			scanner.addToken(tokenComma, nil)
		case '.':
			scanner.addToken(tokenDot, nil)
		case '-':
			scanner.addToken(tokenMinus, nil)
		case '+':
			scanner.addToken(tokenPlus, nil)
		case ';':
			scanner.addToken(tokenSemicolon, nil)
		case '*':
			scanner.addToken(tokenStar, nil)
		// Double character tokens
		case '!':
			if scanner.match('=') {
				scanner.addToken(tokenBangEqual, nil)
			} else {
				scanner.addToken(tokenBang, nil)
			}
		case '=':
			if scanner.match('=') {
				scanner.addToken(tokenEqualEqual, nil)
			} else {
				scanner.addToken(tokenEqual, nil)
			}
		case '<':
			if scanner.match('=') {
				scanner.addToken(tokenLessEqual, nil)
			} else {
				scanner.addToken(tokenLess, nil)
			}
		case '>':
			if scanner.match('=') {
				scanner.addToken(tokenGreaterEqual, nil)
			} else {
				scanner.addToken(tokenGreater, nil)
			}
		// Long lexemes
		case '/':
			if scanner.match('/') {
				// consume the comment, but keep the \n at the end of line so line
				// counting can work correctly
				for scanner.peek() != '\n' && scanner.hasNext() {
					scanner.advance()
				}
			} else if scanner.match('*') {
				scanner.scanMultilineComment()
			} else {
				scanner.addToken(tokenSlash, nil)
			}
		// Literals
		case '"':
			scanner.scanString()
		default:
			if unicode.IsDigit(r) {
				scanner.scanNumber()
			} else if isBeginIdent(r) {
				scanner.scanIdentifier()
			} else {
				scanner.reporter.Report(
					newScanError(scanner.line, "Unexpected character."),
				)
			}
		}
	}
	scanner.tokens = append(
		scanner.tokens,
		newLoxToken(tokenEOF, "", nil, scanner.line),
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
		scanner.addToken(tokenString, literal)
	} else {
		scanner.reporter.Report(
			newScanError(scanner.line, "Unterminated string."),
		)
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
	lexeme := string(scanner.source[scanner.start:scanner.current])
	// NOTE: we're ignoring the error, since we have already verified that the
	// lexeme contains a valid 64-bit floating point.
	literal, _ := strconv.ParseFloat(lexeme, 64)
	scanner.addToken(tokenNumber, literal)
}

func (scanner *Scanner) scanIdentifier() {
	for isAlphanumeric(scanner.peek()) {
		scanner.advance()
	}
	lexeme := string(scanner.source[scanner.start:scanner.current])
	if tokenType, isKeyword := loxKeywords[lexeme]; isKeyword {
		scanner.addToken(tokenType, nil)
	} else {
		scanner.addToken(tokenIdentifier, nil)
	}
}

func (scanner *Scanner) scanMultilineComment() {
	for {
		for scanner.peek() != '*' && scanner.hasNext() {
			if scanner.peek() == '\n' {
				scanner.line++
			}
			scanner.advance()
		}
		if scanner.hasNext() {
			scanner.advance()
			if scanner.peek() == '/' {
				scanner.advance()
				break
			}
		} else {
			scanner.reporter.Report(
				newScanError(
					scanner.line, "Unterminated multiline comment.",
				),
			)
			break
		}
	}
}

// addToken appends the lexeme from `start` to `current` as a token of the given
// type and carries the given literal
func (scanner *Scanner) addToken(typ loxTokenType, literal interface{}) {
	lexeme := string(scanner.source[scanner.start:scanner.current])
	tok := newLoxToken(typ, lexeme, literal, scanner.line)
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
