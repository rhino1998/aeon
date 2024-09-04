package compiler

import (
	"github.com/rhino1998/aeon/pkg/compiler/air"
	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/types"
	"github.com/rhino1998/aeon/pkg/parser"
)

type LiteralValue interface {
	Kind() kinds.Kind
	Value() *air.Value
}

type Literal struct {
	value LiteralValue
	typ   types.Type

	parser.Position
}

func NewLiteral(val LiteralValue) *Literal {
	return &Literal{
		value: val,
		typ:   types.Kind(val.Kind()),
	}
}

func (l *Literal) Value(vs *ValueScope) *air.Value {
	v := l.value.Value()
	if v.Operand.Kind == air.OperandKindString {
		return vs.getString(v.Operand.Value.(air.String))
	}

	return v
}

func (l *Literal) Evaluate() (LiteralValue, error) {
	return l.value, nil
}

func (l *Literal) Type() types.Type {
	return l.typ
}

func (l *Literal) SymbolDependencies(path []*SymbolReference) []*SymbolReference {
	return nil
}

func (l *Literal) SetType(typ types.Type) {
	l.typ = typ
}
