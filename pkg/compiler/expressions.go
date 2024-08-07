package compiler

import (
	"strconv"

	"github.com/rhino1998/aeon/pkg/parser"
)

type Expression interface {
	Type() Type

	WrapError(error) error
}

type DotExpression struct {
	Receiver Expression
	Key      string

	parser.Position
}

func (e *DotExpression) Type() Type {
	switch typ := resolveType(e.Receiver.Type()).(type) {
	case *StructType:
		return UnknownType
	case *MapType:
		return typ.Value()
	case *TupleType:
		index, err := strconv.Atoi(e.Key)
		if err != nil {
			return UnknownType
		}

		if index >= len(typ.Elems()) {
			return UnknownType
		}

		return typ.Elems()[index]
	default:
		return UnknownType
	}
}

type ParenthesizedExpression struct {
	Expression Expression

	parser.Position
}

func (e *ParenthesizedExpression) Type() Type {
	return e.Expression.Type()
}

type ExpressionStatement struct {
	Expression Expression

	parser.Position
}
