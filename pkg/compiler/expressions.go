package compiler

import (
	"fmt"
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
	return resolveDotExpressionType(e, e.Receiver.Type())
}

func resolveDotExpressionType(e *DotExpression, typ Type) Type {
	switch typ := resolveType(typ).(type) {
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
	case *TypeType:
		method, ok := TypeMethod(typ.Type, e.Key)
		if !ok {
			return UnknownType
		}

		return method.Type().(*FunctionType).ToFunction()
	case *InterfaceType:
		method, ok := typ.Methods().Get(e.Key)
		if !ok {
			return UnknownType
		}

		return method.BoundFunctionType()
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
	return resolveIndexExpressionType(e, resolveType(e.Receiver.Type()))
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

func (e *ParenthesizedExpression) Evaluate() (LiteralValue, error) {
	exprConst, ok := e.Expression.(ConstantExpression)
	if !ok {
		return nil, e.Expression.WrapError(fmt.Errorf("expression is not constant %s", e.Expression))
	}

	return exprConst.Evaluate()
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
	Length   ConstantExpression // TODO: enforce known at compile time
	ElemType Type
	Elems    []Expression

	parser.Position
}

func (e *ArrayExpression) Type() Type {
	val, err := e.Length.Evaluate()
	if err != nil {
		panic("bug: unresolvable length in array definition")
	}
	return &ArrayType{
		length: int(val.(Int)),
		elem:   e.ElemType,
	}
}
