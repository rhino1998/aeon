package compiler

import (
	"fmt"
	"slices"

	"github.com/rhino1998/aeon/pkg/compiler/operators"
	"github.com/rhino1998/aeon/pkg/compiler/types"
	"github.com/rhino1998/aeon/pkg/parser"
)

type Variable struct {
	*SymbolReference
	typ  types.Type
	expr Expression

	variadic bool

	parser.Position
}

func NewVariable(name string, typ types.Type, scope *SymbolScope) *Variable {
	return &Variable{
		SymbolReference: &SymbolReference{
			name:  name,
			scope: scope,
		},
		typ: typ,
	}
}

func (v *Variable) Name() string {
	return v.name
}

func (v *Variable) Type() types.Type {
	if v.typ == nil {
		return v.expr.Type()
	}

	return v.typ
}

func (v *Variable) SetType(typ types.Type) {
	v.typ = typ
}

func (v *Variable) String() string {
	return fmt.Sprintf("<var %s %s>", v.name, v.typ)
}

func (v *Variable) Reference() *SymbolReference {
	return v.SymbolReference
}

func (v *Variable) SymbolDependencies(path []*SymbolReference) []*SymbolReference {
	if v.expr == nil {
		return nil
	}
	return v.expr.SymbolDependencies(path)
}

type SymbolReferenceExpression struct {
	*SymbolReference

	parser.Position
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

func (e *SymbolReferenceExpression) SymbolDependencies(path []*SymbolReference) []*SymbolReference {
	if slices.ContainsFunc(path, func(r *SymbolReference) bool {
		return r.QualifiedName() == e.SymbolReference.QualifiedName()
	}) {
		return []*SymbolReference{e.SymbolReference}
	}

	refs := []*SymbolReference{e.SymbolReference}

	val, ok := e.scope.get(e.name)
	if !ok {
		return refs
	}

	symDep, ok := val.(SymbolDependent)
	if !ok {
		return refs
	}

	return append(refs, symDep.SymbolDependencies(append(path, e.SymbolReference))...)
}

func (e *SymbolReferenceExpression) Evaluate() (LiteralValue, error) {
	v := e.Dereference()
	if v == nil {
		return nil, e.WrapError(fmt.Errorf("undefined constant %s", e.name))
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
