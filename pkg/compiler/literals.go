package compiler

import "github.com/rhino1998/aeon/pkg/parser"

type LiteralValue interface {
	Kind() Kind
	Location(*ValueScope) *Location
}

type Literal struct {
	value LiteralValue
	typ   Type

	parser.Position
}

func NewLiteral(val LiteralValue) *Literal {
	return &Literal{
		value: val,
		typ:   TypeKind(val.Kind()),
	}
}

func (l Literal) Value() LiteralValue {
	return l.value
}

func (l Literal) Evaluate() (LiteralValue, error) {
	return l.value, nil
}

func (l Literal) Type() Type {
	return l.typ
}
