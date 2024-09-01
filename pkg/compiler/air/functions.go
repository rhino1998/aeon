package air

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/types"
)

const (
	ExternFuncKindInt Int = 0
	FuncKindInt       Int = 1
)

var ExternFuncTuple = types.NewTuple(
	types.Kind(kinds.Int),
	types.Kind(kinds.String), // funcname
	types.Kind(kinds.String), // filename
	types.Kind(kinds.String), // extern name
)

var FuncTuple = types.NewTuple(
	types.Kind(kinds.Int),
	types.Kind(kinds.String), // funcname
	types.Kind(kinds.String), // filename
	types.AnyPointer,         // ptr into code segment
)

func FunctionStackSize(f *types.Function) (Size, error) {
	var offset Size
	retSize, err := TypeSize(f.Return)
	if err != nil {
		return 0, fmt.Errorf("return size: %v", err)
	}

	offset += retSize

	recvSize, err := TypeSize(f.Receiver)
	if err != nil {
		return 0, fmt.Errorf("receiver size: %v", err)
	}
	offset += recvSize

	for _, param := range f.Parameters {
		paramSize, err := TypeSize(param)
		if err != nil {
			return 0, fmt.Errorf("param size: %v", err)
		}

		offset += paramSize
	}

	return offset, nil
}
