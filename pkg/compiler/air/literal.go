package air

import "github.com/rhino1998/aeon/pkg/compiler/kinds"

type LiteralBound interface {
	Int | Float | String | Bool
	Literal
}

type Literal interface {
	Kind() kinds.Kind
}
