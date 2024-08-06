package compiler

import "github.com/rhino1998/aeon/pkg/parser"

type Variable struct {
	name string
	typ  Type
	expr *any

	global   bool
	escaping bool
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

func (v *Variable) Escaping() bool {
	return v.escaping
}

func (v *Variable) SetEscaping(escaping bool) {
	v.escaping = true
}

type SymbolReferenceExpression struct {
	scope *SymbolScope
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

type VarStatement struct {
	Variable   *Variable
	Expression Expression
	Type       Type

	parser.Position
}

type DeclarationStatement struct {
	Variable   *Variable
	Expression Expression

	parser.Position
}

type PostfixStatement struct {
	Expression Expression
	Operator   Operator

	parser.Position
}
