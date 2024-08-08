package compiler

import "fmt"

type OperandKind int

const (
	OperandKindImmediate OperandKind = 0
	OperandKindRegister  OperandKind = 1
	OperandKindIndirect  OperandKind = 2
	OperandKindOffset    OperandKind = 3
)

func (s OperandKind) MarshalXenon() ([]byte, error) {
	switch s {
	case OperandKindImmediate:
		return []byte("I"), nil
	case OperandKindRegister:
		return []byte("R"), nil
	case OperandKindIndirect:
		return []byte("*"), nil
	case OperandKindOffset:
		return []byte("+"), nil
	default:
		return nil, fmt.Errorf("invalid value source %x", s)
	}
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
	return fmt.Sprintf("%s + %s", o.A, o.B)
}

type Operand struct {
	Kind  OperandKind `xc:"k"`
	Value any         `xc:"v"`
}

func (o *Operand) OffsetReference(offset AddrOffset) *Operand {
	if offset == 0 {
		return o
	}

	return o.AddressOf().ConstOffset(offset).Dereference()
}

func (o *Operand) ConstOffset(offset AddrOffset) *Operand {
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
