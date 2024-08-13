package compiler

import "github.com/rhino1998/aeon/pkg/parser"

type Literal[T any] struct {
	value T
	typ   Type

	parser.Position
}

func NewLiteral[T interface{ Kind() Kind }](val T) *Literal[T] {
	return &Literal[T]{
		value: val,
		typ:   TypeKind(val.Kind()),
	}
}

func (l Literal[T]) Value() T {
	return l.value
}

func (l Literal[T]) Type() Type {
	return l.typ
}
