package compiler

import "github.com/rhino1998/aeon/pkg/parser"

type BooleanLiteral struct {
	value bool
	typ   Type

	parser.Position
}

func (l BooleanLiteral) Type() Type {
	return l.typ
}

type NumericLiteral struct {
	value float64
	typ   Type

	parser.Position
}

func (l NumericLiteral) Type() Type {
	return l.typ
}

type StringLiteral struct {
	value string
	typ   Type

	parser.Position
}

func (l StringLiteral) Type() Type {
	return l.typ
}
