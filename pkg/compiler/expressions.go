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
		field, ok := typ.GetField(e.Key)
		if !ok {
			return UnknownType
		}

		return field.Type
	case *TupleType:
		index, err := strconv.Atoi(e.Key)
		if err != nil {
			return UnknownType
		}

		if index >= len(typ.Elems()) {
			return UnknownType
		}

		return typ.Elems()[index]
	case *DerivedType:
		ftype, ok := typ.BoundMethodType(e.Key)
		if !ok {
			return resolveDotExpressionType(e, typ.Underlying())
		}

		return ftype
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

func resolveIndexExpressionType(_ *IndexExpression, typ Type) Type {
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

type TupleExpression struct {
	Elems []Expression

	parser.Position
}

func (e *TupleExpression) Type() Type {
	elemTyps := make([]Type, 0, len(e.Elems))
	for _, elem := range e.Elems {
		elemTyps = append(elemTyps, elem.Type())
	}

	return &TupleType{
		elems: elemTyps,
	}
}

type ArrayExpression struct {
	Length   *Literal[Int] // TODO: enforce known at compile time
	ElemType Type
	Elems    []Expression

	parser.Position
}

func (e *ArrayExpression) Type() Type {
	return &ArrayType{
		length: int(e.Length.value),
		elem:   e.ElemType,
	}
}
