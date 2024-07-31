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

type AssignmentStatement struct {
	left  Expression
	right Expression

	parser.Position
}

func (s *AssignmentStatement) Left() Expression {
	return s.left
}

func (s *AssignmentStatement) Right() Expression {
	return s.right
}

type VariableStatement struct {
	Variable   Variable
	Expression Expression

	parser.Position
}

type DeclarationStatement struct {
	Variable   Variable
	Expression Expression

	parser.Position
}

type PostfixStatement struct {
	Expression Expression
	Operator   Operator

	parser.Position
}
