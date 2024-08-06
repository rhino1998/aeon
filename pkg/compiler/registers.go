package compiler

import "fmt"

const (
	RegisterZero   Register = 0
	RegisterReturn Register = 1
	RegisterSP     Register = 2
	RegisterFP     Register = 3
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
	OperandRegisterZero   = RegisterOperand(RegisterZero)
	OperandRegisterSP     = RegisterOperand(RegisterSP)
	OperandRegisterFP     = RegisterOperand(RegisterFP)
	OperandRegisterReturn = RegisterOperand(RegisterReturn)
)
