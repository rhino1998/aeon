package air

import (
	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/types"
)

var InterfaceTuple = types.NewTuple(
	types.Kind(kinds.Type),
	types.AnyPointer,
)
