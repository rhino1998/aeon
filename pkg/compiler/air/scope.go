package air

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/types"
)

type Value struct {
	Type    types.Type
	Operand *Operand
}

func (v *Value) AsType(typ types.Type) *Value {
	return &Value{
		Operand: v.Operand,
		Type:    typ,
	}
}

func (v *Value) Value() *Value {
	return &Value{
		Type:    v.Type.(*types.Pointer).Pointee(),
		Operand: v.Operand.Dereference(),
	}
}

func (v *Value) Dereference() (*Value, error) {
	typ, ok := types.Resolve(v.Type).(*types.Pointer)
	if !ok {
		return nil, fmt.Errorf("cannot dereference non-pointer type %s", v.Type)
	}

	return &Value{
		Type:    typ.Pointee(),
		Operand: v.Operand.Dereference(),
	}, nil
}

func (l *Value) InterfaceType() (*Value, error) {
	return l.AsType(InterfaceTuple).IndexTuple(0)
}

func (l *Value) InterfaceValue() (*Value, error) {
	return l.AsType(InterfaceTuple).IndexTuple(1)
}

func (l *Value) IndexTuple(index int) (*Value, error) {
	typ, ok := types.Resolve(l.Type).(*types.Tuple)
	if !ok {
		return nil, fmt.Errorf("cannot index non-tuple type %s", l.Type)
	}

	elemOffset, err := TupleElemOffset(typ, index)
	if err != nil {
		return nil, err
	}

	return &Value{
		Type:    types.Dereference(typ.Elems()[index]),
		Operand: l.Operand.AddressOf().AddConst(elemOffset).Dereference(),
	}, nil
}

func (l *Value) IndexArrayConst(index int) (*Value, error) {
	typ := types.Resolve(l.Type).(*types.Array)

	length := typ.Length()
	if index >= length {
		return nil, fmt.Errorf("compile-time array index %d out of bounds", index)
	}

	elemSize, err := TypeSize(typ.Elem())
	if err != nil {
		return nil, fmt.Errorf("array element size: %v", err)
	}

	return &Value{
		Type:    types.Dereference(typ.Elem()),
		Operand: l.Operand.AddressOf().AddConst(elemSize * Size(index)).Dereference(),
	}, nil
}

func (l *Value) IndexArray(index *Value) (*Value, error) {
	typ := types.Resolve(l.Type).(*types.Array)

	length := typ.Length()

	elemSize, err := TypeSize(typ.Elem())
	if err != nil {
		return nil, fmt.Errorf("array element size: %v", err)
	}

	return &Value{
		Type: types.Dereference(typ.Elem()),
		Operand: l.Operand.AddressOf().Add(
			index.Operand.
				Bound(IntOperand(length)).
				Mul(IntOperand(elemSize))).Dereference(),
	}, nil
}

func (l *Value) IndexSlice(index *Value) (*Value, error) {
	typ := types.Resolve(l.Type).(*types.Slice)

	if index.Type.Kind() != kinds.Int {
		return nil, fmt.Errorf("invalid type for slice index %s", index.Type)
	}

	elemSize, err := TypeSize(typ.Elem())
	if err != nil {
		return nil, fmt.Errorf("slice element size: %v", err)
	}

	return &Value{
		Type:    types.Dereference(typ.Elem()),
		Operand: l.Operand.Add(index.Operand.Bound(l.Operand.AddConst(1)).Mul(IntOperand(elemSize))).Dereference(),
	}, nil
}

func (l *Value) SliceLen() (*Value, error) {
	return &Value{
		Type:    types.Kind(kinds.Int),
		Operand: l.Operand.OffsetReference(1),
	}, nil
}

func (l *Value) SliceCap() (*Value, error) {
	return &Value{
		Type:    types.Kind(kinds.Int),
		Operand: l.Operand.OffsetReference(2),
	}, nil
}

func (l *Value) IndexField(name string) (*Value, error) {
	typ := types.Resolve(l.Type).(*types.Struct)

	field, ok := typ.GetField(name)
	if !ok {
		return nil, fmt.Errorf("struct field %q does not exist", name)
	}

	offset, err := StructFieldOffset(typ, name)
	if err != nil {
		return nil, err
	}

	return &Value{
		Type:    types.Dereference(field.Type),
		Operand: l.Operand.AddressOf().AddConst(offset).Dereference(),
	}, nil
}

func (v *Value) Equal(r *Value) *Value {
	return &Value{
		Type:    types.Kind(kinds.Bool),
		Operand: v.Operand.Equal(r.Operand),
	}
}

func (v *Value) NotEqual(r *Value) *Value {
	return &Value{
		Type:    types.Kind(kinds.Bool),
		Operand: v.Operand.NotEqual(r.Operand),
	}
}

func (v *Value) Add(r *Value) *Value {
	return &Value{
		Type:    v.Type,
		Operand: v.Operand.Add(r.Operand),
	}
}

func (v *Value) AddConst(size Size) *Value {
	return &Value{
		Type:    v.Type,
		Operand: v.Operand.AddConst(size),
	}
}

func (v *Value) Mul(r *Value) *Value {
	return &Value{
		Type:    v.Type,
		Operand: v.Operand.Mul(r.Operand),
	}
}

func (v *Value) Not() *Value {
	return &Value{
		Type:    v.Type,
		Operand: v.Operand.Not(),
	}
}

func (v *Value) AddressOf() *Value {
	return &Value{
		Type:    types.NewPointer(v.Type),
		Operand: v.Operand.AddressOf(),
	}
}
