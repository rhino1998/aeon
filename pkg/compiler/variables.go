package compiler

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler/operators"
	"github.com/rhino1998/aeon/pkg/compiler/types"
	"github.com/rhino1998/aeon/pkg/parser"
)

type Variable struct {
	name string
	typ  types.Type
	expr Expression

	global   bool
	escaping bool
	variadic bool

	parser.Position
}

func (v *Variable) Name() string {
	return v.name
}

func (v *Variable) Type() types.Type {
	return v.typ
}

func (v *Variable) SetType(typ types.Type) {
	v.typ = typ
}

func (v *Variable) Escaping() bool {
	return v.escaping
}

func (v *Variable) SetEscaping(escaping bool) {
	v.escaping = true
}

func (v *Variable) String() string {
	return fmt.Sprintf("<var %s %s>", v.name, v.typ)
}

type SymbolReferenceExpression struct {
	scope *SymbolScope
	name  string

	parser.Position
}

func (e *SymbolReferenceExpression) Name() string {
	return e.name
}

func (e *SymbolReferenceExpression) Type() types.Type {
	v, ok := e.scope.get(e.name)
	if !ok {
		return types.Unknown
	}

	switch v := v.(type) {
	case types.Type:
		return &types.TypeType{
			Type: v,
		}
	case TypedSymbol:
		return v.Type()
	default:
		return types.Unknown
	}

}

func (e *SymbolReferenceExpression) Dereference() Symbol {
	v, ok := e.scope.get(e.name)
	if !ok {
		return nil
	}

	return v
}

func (e *SymbolReferenceExpression) Evaluate() (LiteralValue, error) {
	v := e.Dereference()
	if v == nil {
		return nil, e.WrapError(fmt.Errorf("undefined variable %s", e.name))
	}

	vConst, ok := v.(*Constant)
	if !ok {
		return nil, e.WrapError(fmt.Errorf("symbol is not a constant %s", e.name))
	}

	return vConst.Evaluate()
}

type AssignmentStatement struct {
	Left  Expression
	Right Expression

	parser.Position
}

type VarStatement struct {
	Variable   *Variable
	Expression Expression
	Type       types.Type

	parser.Position
}

type DeclarationStatement struct {
	Variables  []*Variable
	Expression Expression

	parser.Position
}

type PostfixStatement struct {
	Expression Expression
	Operator   operators.Operator

	parser.Position
}
