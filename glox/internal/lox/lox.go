package lox

import (
	"fmt"
	"strconv"
	"unicode"
)

func Stringify(v interface{}) string {
	switch v := v.(type) {
	case nil:
		return fmt.Sprint("nil")
	case float64:
		return fmt.Sprint(strconv.FormatFloat(v, 'f', -1, 64))
	default:
		return fmt.Sprint(v)
	}
}

func Truthy(value interface{}) bool {
	if value == nil {
		return false
	}
	if v, ok := value.(bool); ok {
		return v
	}
	return true
}

func isAlphanumeric(r rune) bool {
	return unicode.IsLetter(r) || unicode.IsDigit(r)
}

func isBeginIdent(r rune) bool {
	return unicode.IsLetter(r) || r == '_'
}
