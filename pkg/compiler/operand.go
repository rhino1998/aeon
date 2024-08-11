package compiler

import "fmt"

type OperandKind int

const (
	OperandKindUnknown OperandKind = iota
	OperandKindImmediate
	OperandKindRegister
	OperandKindIndirect
	OperandKindOffset
	OperandKindStride
	OperandKindVTableLookup
)

func (s OperandKind) MarshalXenon() ([]byte, error) {
	switch s {
	case OperandKindImmediate:
		return []byte("I"), nil
	case OperandKindRegister:
		return []byte("R"), nil
	case OperandKindIndirect:
		return []byte("@"), nil
	case OperandKindOffset:
		return []byte("+"), nil
	case OperandKindStride:
		return []byte("*"), nil
	case OperandKindVTableLookup:
		return []byte("V"), nil
	default:
		return nil, fmt.Errorf("invalid operand kind %x", s)
	}
}

type VTableLookup struct {
	Type   *Operand `xc:"t"`
	Method *Operand `xc:"m"`
}

type Indirect struct {
	Ptr *Operand `xc:"p"`
}

func (i Indirect) String() string {
	return fmt.Sprintf("[%s]", i.Ptr)
}

type Offset struct {
	A *Operand `xc:"a"`
	B *Operand `xc:"b"`
}

func (o Offset) String() string {
	return fmt.Sprintf("(%s + %s)", o.A, o.B)
}

type Stride struct {
	A *Operand `xc:"a"`
	B *Operand `xc:"b"`
}

func (o Stride) String() string {
	return fmt.Sprintf("(%s*%s)", o.A, o.B)
}

type Operand struct {
	Kind  OperandKind `xc:"k"`
	Value any         `xc:"v"`
}

func NewVTableLookup(typ, method *Operand) *Operand {
	return &Operand{
		Kind: OperandKindVTableLookup,
		Value: VTableLookup{
			Type:   typ,
			Method: method,
		},
	}
}

func (o *Operand) VTableLookup(method *Operand) *Operand {
	return &Operand{
		Kind: OperandKindVTableLookup,
		Value: VTableLookup{
			Type:   o,
			Method: method,
		},
	}
}

func (o *Operand) OffsetReference(offset Size) *Operand {
	if offset == 0 {
		return o
	}

	return o.AddressOf().ConstOffset(offset).Dereference()
}

func (o *Operand) ConstOffset(offset Size) *Operand {
	if offset == 0 {
		return o
	}

	return o.Offset(ImmediateOperand(Int(offset)))
}

func (o *Operand) Offset(offset *Operand) *Operand {
	return &Operand{
		Kind: OperandKindOffset,
		Value: Offset{
			A: o,
			B: offset,
		},
	}
}

func (o *Operand) Stride(offset *Operand) *Operand {
	return &Operand{
		Kind: OperandKindStride,
		Value: Stride{
			A: o,
			B: offset,
		},
	}
}

func (o *Operand) AddressOf() *Operand {
	return o.Value.(Indirect).Ptr
}

func (o *Operand) Dereference() *Operand {
	return &Operand{
		Kind: OperandKindIndirect,
		Value: Indirect{
			Ptr: o,
		},
	}
}

func (o Operand) String() string {
	return fmt.Sprintf("%s", o.Value)
}

func ImmediateOperand(imm Immediate) *Operand {
	return &Operand{
		Value: imm,
		Kind:  OperandKindImmediate,
	}
}
