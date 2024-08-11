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
	return resolveDotExpressionType(e, BaseType(e.Receiver.Type()))
}

func resolveDotExpressionType(e *DotExpression, typ Type) Type {
	switch typ := typ.(type) {
	case *StructType:
		return UnknownType
	case *TupleType:
		index, err := strconv.Atoi(e.Key)
		if err != nil {
			return UnknownType
		}

		if index >= len(typ.Elems()) {
			return UnknownType
		}

		return typ.Elems()[index]
	case *PointerType:
		return resolveDotExpressionType(e, typ.Pointee())
	default:
		return UnknownType
	}
}

type IndexExpression struct {
	Receiver Expression
	Index    Expression

	parser.Position
}

func (e *IndexExpression) Type() Type {
	return resolveIndexExpressionType(e, BaseType(e.Receiver.Type()))
}

func resolveIndexExpressionType(e *IndexExpression, typ Type) Type {
	switch typ := typ.(type) {
	case *MapType:
		return typ.Value()
	case *SliceType:
		return typ.Elem()
	case *ArrayType:
		return typ.Elem()
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
