package compiler

import "github.com/rhino1998/aeon/pkg/parser"

type Literal[T Immediate] struct {
	value T
	typ   Type

	parser.Position
}

func NewLiteral[T Immediate](imm T) *Literal[T] {
	return &Literal[T]{
		value: imm,
		typ:   TypeKind(imm.Kind()),
	}
}

func (l Literal[T]) Value() T {
	return l.value
}

func (l Literal[T]) Type() Type {
	return l.typ
}
