package compiler

import (
	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/types"
)

var heapAllocSliceType = types.NewSlice(types.Kind(kinds.Int))
var heapAllocSliceName = "#heapAllocs"
var heapStrAllocSliceName = "#heapStrAllocs"

var heapAllocs = NewVariable(heapAllocSliceName, heapAllocSliceType, nil)
var heapStrAllocs = NewVariable(heapStrAllocSliceName, heapAllocSliceType, nil)
