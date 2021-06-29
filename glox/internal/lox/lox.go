package lox

import (
	"fmt"
	"strconv"
	"unicode"
)

func stringify(v interface{}) string {
	switch v := v.(type) {
	case nil:
		return fmt.Sprint("nil")
	case float64:
		return fmt.Sprint(strconv.FormatFloat(v, 'f', -1, 64))
	default:
		return fmt.Sprintf("%v\n", v)
	}
}

func isTruthy(value interface{}) bool {
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
