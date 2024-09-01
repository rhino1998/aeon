package air

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler/types"
)

func TupleElemOffset(tuple *types.Tuple, index int) (Size, error) {
	if index >= len(tuple.Elems()) {
		return 0, fmt.Errorf("index out of bounds")
	}

	var offset Size
	for i := 0; i < index; i++ {
		elemSize, err := TypeSize(tuple.Elems()[i])
		if err != nil {
			return 0, fmt.Errorf("tuple element size: %v", err)
		}

		offset += elemSize
	}

	return offset, nil
}
