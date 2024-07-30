package compiler

type Function struct {
	name string

	parameters []*Variable
	ret        Type
	body       []Statement

	scope *Scope
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

type CallExpression struct {
	function Expression
	args     []Expression
}

func (e *CallExpression) Type() Type {
	return e.function.Type()
}
