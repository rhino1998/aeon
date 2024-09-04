package compiler

import (
	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/types"
)

var heapAllocSliceType = types.NewSlice(types.Kind(kinds.Int))
