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

type SymbolReferenceExpression struct {
	scope *Scope
	name  string

	parser.Position
}

func (e *SymbolReferenceExpression) Type() Type {
	v, ok := e.scope.getTypedSymbol(e.name)
	if !ok {
		return UnknownType
	}

	return v.Type()
}

type AssignmentStatement struct {
	left  Expression
	right Expression
}

type VariableStatement struct {
	Variable
	expression Expression
}

type DeclarationStatement struct {
	Variable
	expression Expression
}
