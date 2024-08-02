package compiler

import "github.com/rhino1998/aeon/pkg/parser"

type Function struct {
	name string
	pkg  *Package

	parameters []*Variable
	ret        Type
	body       []Statement

	scope *Scope
}

func (f *Function) Package() *Package {
	return f.pkg
}

func (f *Function) Name() string {
	return f.name
}

func (f *Function) Type() Type {
	var scope string
	if f.scope.parent != nil {
		scope = f.scope.parent.name
	}

	var paramTypes []Type
	for _, param := range f.parameters {
		paramTypes = append(paramTypes, param.Type())
	}

	return &FunctionType{
		name:       f.name,
		scope:      scope,
		parameters: paramTypes,
		ret:        f.ret,
	}
}

func (f *Function) Parameters() []*Variable {
	return f.parameters
}

func (f *Function) Return() Type {
	return f.ret
}

func (f *Function) Body() []Statement {
	return f.body
}

type CallExpression struct {
	Function Expression
	Args     []Expression

	parser.Position
}

func (e *CallExpression) Type() Type {
	return BaseType(e.Function.Type()).(*FunctionType).Return()
}

type ReturnStatement struct {
	Expression Expression

	parser.Position
}
