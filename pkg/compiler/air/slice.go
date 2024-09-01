package air

import (
	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/types"
)

var SliceTuple = types.NewTuple(
	types.AnyPointer,
	types.Kind(kinds.Int),
	types.Kind(kinds.Int),
)
