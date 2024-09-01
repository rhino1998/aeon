package air

import "github.com/rhino1998/aeon/pkg/compiler/types"

type Variable interface {
	Name() string
	Type() types.Type
}
