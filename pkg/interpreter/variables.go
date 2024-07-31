package interpreter

import "github.com/rhino1998/aeon/pkg/compiler"

type Variable struct {
	typ   compiler.Type
	value any
}

func NewVariable(typ compiler.Type, val Value) *Variable {
	var raw any
	if val == nil {
		// TODO: zero value
	} else {
		raw = val.Raw()
	}

	return &Variable{
		typ:   typ,
		value: raw,
	}
}

func (v *Variable) Type() compiler.Type {
	return v.typ
}

func (v *Variable) Raw() any {
	return v.value
}

func (v *Variable) Set(val Value) error {
	v.value = val.Raw()

	return nil
}

func (v *Variable) AsType(t compiler.Type) any {
	return v.value
}

type Constant struct {
	typ   compiler.Type
	value any
}

func NewConstant(typ compiler.Type, val any) Constant {
	return Constant{
		typ:   typ,
		value: val,
	}
}

func (c Constant) Type() compiler.Type {
	return c.typ
}

func (c Constant) Raw() any {
	return c.value
}
