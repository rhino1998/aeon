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
	name  string
	scope *SymbolScope
	typ   types.Type
	ConstantExpression

	parser.Position
}

func (c *Constant) Reference() *SymbolReference {
	return &SymbolReference{
		name:  c.name,
		scope: c.scope,
	}
}

func (c *Constant) QualifiedName() string {
	return c.scope.qualifiedSymbolName(c.name)
}

func (c *Constant) Name() string {
	return c.name
}

func (c *Constant) Type() types.Type {
	return c.typ
}

func (c *Constant) WrapError(err error) error {
	return c.Position.WrapError(err)
}

type ConstantValue interface {
	Kind() kinds.Kind
}
