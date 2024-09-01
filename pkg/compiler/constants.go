package compiler

import (
	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/types"
	"github.com/rhino1998/aeon/pkg/parser"
)

type ConstantExpression interface {
	Expression

	Evaluate() (LiteralValue, error)
}

type Constant struct {
	name string
	typ  types.Type
	ConstantExpression

	parser.Position
}

func (v *Constant) Name() string {
	return v.name
}

func (v *Constant) Type() types.Type {
	return v.typ
}

type ConstantValue interface {
	Kind() kinds.Kind
}
