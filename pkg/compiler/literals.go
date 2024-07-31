package compiler

import "github.com/rhino1998/aeon/pkg/parser"

type Literal[T any] struct {
	value T
	typ   Type

	parser.Position
}

func (l Literal[T]) Value() T {
	return l.value
}

func (l Literal[T]) Type() Type {
	return l.typ
}
