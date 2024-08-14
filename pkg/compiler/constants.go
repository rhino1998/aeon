package compiler

import "github.com/rhino1998/aeon/pkg/parser"

type ConstantExpression interface {
	Expression

	Evaluate() (LiteralValue, error)
}

type Constant struct {
	name string
	typ  Type
	ConstantExpression

	parser.Position
}

func (v *Constant) Name() string {
	return v.name
}

func (v *Constant) Type() Type {
	return v.typ
}

type ConstantValue interface {
	Kind() Kind
}
