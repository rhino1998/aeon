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

type TypeLiteralExpression struct {
	typ   Type
	Elems []Expression

	parser.Position
}

func (e *TypeLiteralExpression) Type() Type {
	return e.typ
}

type UnknownExpression struct {
	parser.Expr
}

func (e *UnknownExpression) Type() Type {
	return UnknownType
}

type SpreadExpression struct {
	Expr Expression

	parser.Position
}

func (e *SpreadExpression) Type() Type {
	if e.Expr.Type().Kind() != KindSlice {
		return &VariadicType{
			elem: UnknownType,

			Position: e.Position,
		}
	}
	return resolveType(e.Expr.Type()).(*SliceType).AsVariadic()
}

type ErrorReturnExpression struct {
	Function *Function
	Expr     Expression

	parser.Position
}

func (e *ErrorReturnExpression) Type() Type {
	switch e.Expr.Type().Kind() {
	case KindInterface:
		iface := e.Expr.Type().(*InterfaceType)
		if !TypeError.Underlying().(*InterfaceType).ImplementedBy(iface) {
			return UnknownType
		}

		return TypeVoid
	case KindTuple:
		tupleType := e.Expr.Type().(*TupleType)
		iface, ok := resolveType(tupleType.Elems()[len(tupleType.Elems())-1]).(*InterfaceType)
		if !ok {
			return UnknownType
		}

		if !TypeError.Underlying().(*InterfaceType).ImplementedBy(iface) {
			return UnknownType
		}

		if len(tupleType.Elems()) == 1 {
			return TypeVoid
		}

		if len(tupleType.Elems()) == 2 {
			return tupleType.Elems()[0]
		}

		return NewTupleType(tupleType.Elems()[:len(tupleType.Elems())-1]...)
	default:
		return UnknownType
	}
}

type ErrorHandlerExpression struct {
	Function *Function

	Expr    Expression
	Handler Expression
	parser.Position
}

func (e *ErrorHandlerExpression) Type() Type {
	return e.Expr.Type()
}
