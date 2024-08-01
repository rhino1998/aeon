package compiler

import "github.com/rhino1998/aeon/pkg/parser"

type Variable struct {
	name string
	typ  Type
	expr *any
}

func (v *Variable) Name() string {
	return v.name
}

func (v *Variable) Type() Type {
	return v.typ
}

func (v *Variable) SetType(typ Type) {
	v.typ = typ
}

type SymbolReferenceExpression struct {
	scope *Scope
	name  string

	parser.Position
}

func (e *SymbolReferenceExpression) Name() string {
	return e.name
}

func (e *SymbolReferenceExpression) Type() Type {
	v, ok := e.scope.getTypedSymbol(e.name)
	if !ok {
		return UnknownType
	}

	return v.Type()
}

func (e *SymbolReferenceExpression) Dereference() TypedSymbol {
	v, ok := e.scope.getTypedSymbol(e.name)
	if !ok {
		return nil
	}

	return v
}

type AssignmentStatement struct {
	Left  Expression
	Right Expression

	parser.Position
}

type VariableStatement struct {
	Variable   *Variable
	Expression Expression
	Type       Type

	parser.Position
}

type DeclarationStatement struct {
	Variable   *Variable
	Expression Expression
	Type       Type

	parser.Position
}

type PostfixStatement struct {
	Expression Expression
	Operator   Operator

	parser.Position
}
