package compiler

import (
	"fmt"
	"strconv"

	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/types"
	"github.com/rhino1998/aeon/pkg/parser"
)

type Expression interface {
	Type() types.Type

	WrapError(error) error
}

type DotExpression struct {
	Receiver Expression
	Key      string

	parser.Position
}

func (e *DotExpression) Type() types.Type {
	return resolveDotExpressionType(e, e.Receiver.Type())
}

func resolveDotExpressionType(e *DotExpression, typ types.Type) types.Type {
	switch typ := types.Resolve(typ).(type) {
	case *types.Struct:
		field, ok := typ.GetField(e.Key)
		if !ok {
			return types.Unknown
		}

		return field.Type
	case *types.Tuple:
		index, err := strconv.Atoi(e.Key)
		if err != nil {
			return types.Unknown
		}

		if index >= len(typ.Elems()) {
			return types.Unknown
		}

		return typ.Elems()[index]
	case *types.Derived:
		method, ok := types.Methods(typ).Get(e.Key)
		if !ok {
			return resolveDotExpressionType(e, typ.Underlying())
		}

		return method.BoundFunction()
	case *types.Pointer:
		return resolveDotExpressionType(e, typ.Pointee())
	case *types.TypeType:
		method, ok := types.Methods(typ.Type).Get(e.Key)
		if !ok {
			return types.Unknown
		}

		return method.UnboundFunction()
	case *types.Interface:
		method, ok := typ.Methods().Get(e.Key)
		if !ok {
			return types.Unknown
		}

		return method.BoundFunction()
	default:
		return types.Unknown
	}
}

type IndexExpression struct {
	Receiver Expression
	Index    Expression

	parser.Position
}

func (e *IndexExpression) Type() types.Type {
	return resolveIndexExpressionType(e, types.Resolve(e.Receiver.Type()))
}

func resolveIndexExpressionType(_ *IndexExpression, typ types.Type) types.Type {
	switch typ := typ.(type) {
	case *types.Map:
		return typ.Value()
	case *types.Slice:
		return typ.Elem()
	case *types.Array:
		return typ.Elem()
	default:
		return types.Unknown
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

func (e *ParenthesizedExpression) Type() types.Type {
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

func (e *TupleExpression) Type() types.Type {
	elemTyps := make([]types.Type, 0, len(e.Elems))
	for _, elem := range e.Elems {
		elemTyps = append(elemTyps, elem.Type())
	}

	return types.NewTuple(elemTyps...)
}

type TypeLiteralExpression struct {
	typ   types.Type
	Elems []Expression

	parser.Position
}

func (e *TypeLiteralExpression) Type() types.Type {
	return e.typ
}

type UnknownExpression struct {
	parser.Expr
}

func (e *UnknownExpression) Type() types.Type {
	return types.Unknown
}

type SpreadExpression struct {
	Expr Expression

	parser.Position
}

func (e *SpreadExpression) Type() types.Type {
	if e.Expr.Type().Kind() != kinds.Slice {
		return types.NewVariadic(types.Unknown)
	}
	return types.Resolve(e.Expr.Type()).(*types.Slice).AsVariadic()
}

type ErrorReturnExpression struct {
	Function *Function
	Expr     Expression

	parser.Position
}

func (e *ErrorReturnExpression) Type() types.Type {
	switch e.Expr.Type().Kind() {
	case kinds.Interface:
		iface := e.Expr.Type().(*types.Interface)
		if !TypeError.Underlying().(*types.Interface).ImplementedBy(iface) {
			return types.Unknown
		}

		return types.Void
	case kinds.Tuple:
		tupleType := e.Expr.Type().(*types.Tuple)
		iface, ok := types.Resolve(tupleType.Elems()[len(tupleType.Elems())-1]).(*types.Interface)
		if !ok {
			return types.Unknown
		}

		if !TypeError.Underlying().(*types.Interface).ImplementedBy(iface) {
			return types.Unknown
		}

		if len(tupleType.Elems()) == 1 {
			return types.Void
		}

		if len(tupleType.Elems()) == 2 {
			return tupleType.Elems()[0]
		}

		return types.NewTuple(tupleType.Elems()[:len(tupleType.Elems())-1]...)
	default:
		return types.Unknown
	}
}

var errorHandlerFunctionType = types.NewFunction(types.Void, []types.Type{TypeError}, TypeError)

type ErrorHandlerExpression struct {
	Function *Function

	Expr    Expression
	Handler Expression
	parser.Position
}

func (e *ErrorHandlerExpression) Type() types.Type {
	return e.Expr.Type()
}

type NilExpression struct {
	typ types.Type

	parser.Position
}

func (e *NilExpression) Type() types.Type {
	return e.typ
}

type InterfaceCoercionExpression struct {
	Interface  *types.Interface
	Expression Expression

	parser.Position
}

func (e *InterfaceCoercionExpression) Type() types.Type {
	return e.Interface
}

func (e *InterfaceCoercionExpression) WrapError(err error) error {
	return e.Expression.WrapError(err)
}

type TypeExpression struct {
	typ types.Type

	parser.Position
}

func (t *TypeExpression) Type() types.Type {
	return &types.TypeType{Type: t.typ}
}
