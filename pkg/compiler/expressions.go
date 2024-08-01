package compiler

import "github.com/rhino1998/aeon/pkg/parser"

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
