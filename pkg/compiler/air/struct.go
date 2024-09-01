package air

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler/types"
)

func StructFieldOffset(struc *types.Struct, fieldName string) (Size, error) {
	var offset Size
	for _, field := range struc.Fields() {
		elemSize, err := TypeSize(field.Type)
		if err != nil {
			return 0, fmt.Errorf("tuple element size: %v", err)
		}

		if field.Name == fieldName {
			return offset, nil
		}

		offset += elemSize
	}

	return 0, fmt.Errorf("field %s not found in struct", fieldName)
}
