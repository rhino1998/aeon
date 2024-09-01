package air

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/types"
)

func TypeSize(typ types.Type) (Size, error) {
	switch t := types.Resolve(typ).(type) {
	case nil:
		panic("nil type")
	case types.Kind:
		return KindSize(t.Kind())
	case *types.Pointer:
		return 1, nil
	case *types.Function:
		return 1, nil
	case *types.Slice:
		return TypeSize(SliceTuple)
	case *types.Variadic:
		return TypeSize(SliceTuple)
	case *types.Interface:
		return TypeSize(InterfaceTuple)
	case *types.Array:
		elemSize, err := TypeSize(t.Elem())
		if err != nil {
			return 0, fmt.Errorf("array element size: %v", err)
		}
		return Size(t.Length()) * elemSize, nil
	case *types.Struct:
		var size Size
		for _, field := range t.Fields() {
			fieldSize, err := TypeSize(field.Type)
			if err != nil {
				return 0, fmt.Errorf("struct field size: %v", err)
			}

			size += fieldSize
		}

		return size, nil
	case *types.Tuple:
		var size Size
		for _, elem := range t.Elems() {
			elemSize, err := TypeSize(elem)
			if err != nil {
				return 0, fmt.Errorf("tuple element size: %v", err)
			}
			size += elemSize
		}

		return size, nil
	default:
		return 0, fmt.Errorf("unknown size for type %v", typ)
	}
}

func KindSize(kind kinds.Kind) (Size, error) {
	switch kind {
	case kinds.Void, kinds.Nil:
		return 0, nil
	case kinds.Bool, kinds.Int, kinds.Float, kinds.String, kinds.Pointer, kinds.Type:
		return 1, nil
	default:
		return 0, fmt.Errorf("unknown size for kind %v", kind)
	}
}
