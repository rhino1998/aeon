package compiler

import "fmt"

const (
	RegisterZero Register = 0
	RegisterSP   Register = 1
	RegisterFP   Register = 2
)

type Register int64

func (r Register) String() string {
	return fmt.Sprintf("reg%02x", int64(r))
}

func RegisterOperand(reg Register) *Operand {
	return &Operand{
		Kind:  OperandKindRegister,
		Value: reg,
	}
}

var (
	OperandRegisterZero = RegisterOperand(RegisterZero)
	OperandRegisterSP   = RegisterOperand(RegisterSP)
	OperandRegisterFP   = RegisterOperand(RegisterFP)
)
