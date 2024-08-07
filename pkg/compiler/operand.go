package compiler

import "fmt"

type OperandKind int

const (
	OperandKindImmediate OperandKind = 0
	OperandKindRegister  OperandKind = 1
	OperandKindIndirect  OperandKind = 2
	OperandKindExtern    OperandKind = 3
)

func (s OperandKind) MarshalXenon() ([]byte, error) {
	switch s {
	case OperandKindImmediate:
		return []byte("I"), nil
	case OperandKindRegister:
		return []byte("R"), nil
	case OperandKindIndirect:
		return []byte("*"), nil
	default:
		return nil, fmt.Errorf("invalid value source %x", s)
	}
}

type Indirect struct {
	Base   *Operand   `xc:"b"`
	Offset AddrOffset `xc:"o"`
}

func (i Indirect) String() string {
	return fmt.Sprintf("[%s %s]", i.Base, i.Offset)
}

type Operand struct {
	Kind  OperandKind `xc:"k"`
	Value any         `xc:"v"`
}

func (o *Operand) Offset(offset AddrOffset) *Operand {
	if offset == 0 {
		return o
	}

	return &Operand{
		Kind: OperandKindIndirect,
		Value: Indirect{
			Base:   o.Value.(Indirect).Base,
			Offset: o.Value.(Indirect).Offset + offset,
		},
	}
}

func (o *Operand) Dereference() *Operand {
	return &Operand{
		Kind: OperandKindIndirect,
		Value: Indirect{
			Base:   o,
			Offset: 0,
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

type OperandExpression struct {
	Operand *Operand
	typ     Type
}

func (o *OperandExpression) Type() Type {
	return o.typ
}

func newOperandExpression(o *Operand, kind Kind) *OperandExpression {
	return &OperandExpression{
		Operand: o,
		typ:     KindType(kind),
	}
}

func (o *OperandExpression) WrapError(err error) error {
	return fmt.Errorf("compile-time: %w", err)
}
