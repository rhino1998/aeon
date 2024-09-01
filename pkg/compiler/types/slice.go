package types

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/parser"
)

type Variadic struct {
	elem Type

	parser.Position
}

func NewVariadic(elem Type) *Variadic {
	return &Variadic{elem: elem}
}

func (t *Variadic) String() string {
	return fmt.Sprintf("...%s", t.elem.String())
}

func (t *Variadic) GlobalName() Name {
	return Name(fmt.Sprintf("...%s", string(t.elem.GlobalName())))
}

func (t *Variadic) Kind() kinds.Kind {
	return kinds.Variadic
}

func (t *Variadic) Elem() Type {
	return t.elem
}

func (t *Variadic) AsSlice() *Slice {
	return &Slice{elem: t.elem, Position: t.Position}
}
