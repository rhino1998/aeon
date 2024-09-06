package abc

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler/air"
	"github.com/rhino1998/aeon/pkg/compiler/operators"
)

type Program struct {
	Bytecode Snippet
	Labels   map[air.Label]int
}

type Snippet []Instruction

func (s *Snippet) Length() int {
	var l int
	for _, ins := range *s {
		l += len(ins)
	}

	return l
}

func (s *Snippet) Add(f ...Instruction) {
	*s = append(*s, f...)
}

type Instruction []float64

func (i *Instruction) Add(f ...float64) {
	*i = append(*i, f...)
}

const (
	Nop   = 0x00
	Mov   = 0x01
	Jmp   = 0x02
	Binop = 0x03
	Unop  = 0x04
	Call  = 0x05
	Ret   = 0x06
	Alc   = 0x07
	App   = 0x08
)

func Compile(instructions []air.Instruction) Program {
	p := Program{
		Labels: make(map[air.Label]int),
	}

	var offset int
	for _, instruction := range instructions {
		switch instruction := instruction.(type) {
		case air.LabelledInstruction:
			for _, label := range instruction.Labels {
				if _, ok := p.Labels[label]; ok {
					panic(fmt.Sprintf("bug: duplicate label %s", label))
				}
				p.Labels[label] = offset
			}
		}

		offset += InstructionSize(instruction)
	}

	offset = 0
	for _, instruction := range instructions {
		ins := p.compileInstruction(instruction, offset)
		p.Bytecode.Add(ins)
		offset += len(ins)
	}

	return p
}

func (p *Program) compileInstruction(instruction air.Instruction, current int) Instruction {
	var s Instruction
	switch ins := instruction.(type) {
	case air.LabelledInstruction:
		return p.compileInstruction(ins.Instruction, current)
	case air.Nop:
		s.Add(Nop)
	case air.Mov:
		s.Add(Mov)
		s.Add(float64(ins.Size))
		dst := p.compileOperand(ins, ins.Dst, 0)
		s.Add(float64(len(dst)))
		s.Add(dst...)
		src := p.compileOperand(ins, ins.Src, 0)
		s.Add(float64(len(src)))
		s.Add(src...)
	case air.Jmp:
		s.Add(Jmp)
		target := p.compileOperand(ins, ins.Target, current)
		s.Add(float64(len(target)))
		s.Add(target...)
		cond := p.compileOperand(ins, ins.Cond, current)
		s.Add(float64(len(cond)))
		s.Add(cond...)
	case air.BinOp:
		s.Add(Binop)
		dst := p.compileOperand(ins, ins.Dst, 0)
		s.Add(float64(len(dst)))
		s.Add(dst...)
		s.Add(float64(ins.Kind))
		s.Add(float64(BinaryOperatorUOP(ins.Op)))
		left := p.compileOperand(ins, ins.Left, 0)
		s.Add(float64(len(left)))
		s.Add(left...)
		right := p.compileOperand(ins, ins.Right, 0)
		s.Add(float64(len(right)))
		s.Add(right...)
	case air.UnOp:
		s.Add(Unop)
		dst := p.compileOperand(ins, ins.Dst, 0)
		s.Add(float64(len(dst)))
		s.Add(dst...)
		s.Add(float64(ins.Kind))
		s.Add(float64(UnaryOperatorUOP(ins.Op)))
		src := p.compileOperand(ins, ins.Src, 0)
		s.Add(float64(len(src)))
		s.Add(src...)
	case air.Cal:
		s.Add(Call)
		s.Add(float64(ins.Line))
		fun := p.compileOperand(ins, ins.Func, 0)
		s.Add(float64(len(fun)))
		s.Add(fun...)
	case air.Ret:
		s.Add(Ret)
		s.Add(float64(ins.Args))
	case air.Alc:
		s.Add(Alc)
		dst := p.compileOperand(ins, ins.Dst, 0)
		s.Add(float64(len(dst)))
		s.Add(dst...)
		size := p.compileOperand(ins, ins.Size, 0)
		s.Add(float64(len(size)))
		s.Add(size...)
	case air.App:
		s.Add(App)
		dst := p.compileOperand(ins, ins.Dst, 0)
		s.Add(float64(len(dst)))
		s.Add(dst...)
		src := p.compileOperand(ins, ins.Src, 0)
		s.Add(float64(len(src)))
		s.Add(src...)
		elem := p.compileOperand(ins, ins.Elem, 0)
		s.Add(float64(len(elem)))
		s.Add(elem...)
		s.Add(float64(ins.Size))
	default:
		panic(fmt.Errorf("invalid instruction type %T", ins))
	}

	return s
}

func (p *Program) compileOperand(ins air.Instruction, operand *air.Operand, current int) Instruction {
	var s Instruction
	switch operand.Kind {
	case air.OperandKindImmediate:
		s.Add(UOPImmediate)
		switch v := operand.Value.(type) {
		case air.Int:
			s.Add(float64(v))
		case air.Float:
			s.Add(float64(v))
		case air.String:
			panic("unresolved string")
		case air.Bool:
			if v {
				s.Add(1)
			} else {
				s.Add(0)
			}
		default:
			panic(fmt.Errorf("invalid immediate type %T", v))
		}
	case air.OperandKindRegister:
		s.Add(UOPImmediate, float64(operand.Value.(air.Register)), UOPRegister)
	case air.OperandKindIndirect:
		s.Add(p.compileOperand(ins, operand.Value.(air.Indirect).Ptr, current)...)
		s.Add(UOPIndirect)
	case air.OperandKindBinary:
		s.Add(p.compileOperand(ins, operand.Value.(air.BinaryOperand).Left, current)...)
		s.Add(p.compileOperand(ins, operand.Value.(air.BinaryOperand).Right, current)...)
		s.Add(BinaryOperatorUOP(operand.Value.(air.BinaryOperand).Op))
	case air.OperandKindUnary:
		s.Add(p.compileOperand(ins, operand.Value.(air.UnaryOperand).A, current)...)
		s.Add(UnaryOperatorUOP(operand.Value.(air.UnaryOperand).Op))
	case air.OperandKindVTableLookup:
		s.Add(p.compileOperand(ins, operand.Value.(air.VTableLookup).Type, current)...)
		s.Add(p.compileOperand(ins, operand.Value.(air.VTableLookup).Method, current)...)
		s.Add(UOPVTableLookup)
	case air.OperandKindLabel:
		label, ok := p.Labels[operand.Value.(air.Label)]
		if !ok {
			panic(fmt.Errorf("unresolved label %s", operand.Value.(air.Label)))
		}
		s.Add(UOPImmediate, float64(label-current))
	default:
		panic(fmt.Errorf("invalid operand kind %q", operand.Kind))
	}
	return s
}

func InstructionSize(instruction air.Instruction) int {
	switch instruction := instruction.(type) {
	case air.LabelledInstruction:
		return InstructionSize(instruction.Instruction)
	case air.Nop:
		return 1
	case air.Mov:
		return 1 + (1) + (1 + OperandSize(instruction.Dst)) + (1 + OperandSize(instruction.Src))
	case air.Jmp:
		return 1 + (1 + OperandSize(instruction.Target)) + (1 + OperandSize(instruction.Cond))
	case air.BinOp:
		return 1 + (1 + OperandSize(instruction.Dst)) + (1 + 1) + (1 + OperandSize(instruction.Left) + 1 + OperandSize(instruction.Right))
	case air.UnOp:
		return 1 + (1 + OperandSize(instruction.Dst)) + (1 + 1) + (1 + OperandSize(instruction.Src))
	case air.Cal:
		return 1 + (1 /*line*/) + 1 + OperandSize(instruction.Func)
	case air.Ret:
		return 1 + (1)
	case air.Alc:
		return 1 + (1 + OperandSize(instruction.Dst)) + (1 + OperandSize(instruction.Size))
	case air.App:
		return 1 + (1 + OperandSize(instruction.Dst)) + (1 + OperandSize(instruction.Src)) + (1 + OperandSize(instruction.Elem) + 1)
	default:
		panic(fmt.Errorf("invalid instruction type %T", instruction))
	}
}

func OperandSize(operand *air.Operand) int {
	switch operand.Kind {
	case air.OperandKindImmediate:
		return 2
	case air.OperandKindRegister:
		return 3
	case air.OperandKindIndirect:
		return 1 + OperandSize(operand.Value.(air.Indirect).Ptr)
	case air.OperandKindBinary:
		return 1 + OperandSize(operand.Value.(air.BinaryOperand).Left) + OperandSize(operand.Value.(air.BinaryOperand).Right)
	case air.OperandKindUnary:
		return 1 + OperandSize(operand.Value.(air.UnaryOperand).A)
	case air.OperandKindVTableLookup:
		return 1 + OperandSize(operand.Value.(air.VTableLookup).Type) + OperandSize(operand.Value.(air.VTableLookup).Method)

	case air.OperandKindLabel:
		return 2
	default:
		panic(fmt.Errorf("invalid operand kind %q", operand.Kind))
	}
}

const (
	UOPImmediate          = 0x00
	UOPRegister           = 0x01
	UOPIndirect           = 0x02
	UOPVTableLookup       = 0x03
	UOPAddition           = 0x04
	UOPSubtraction        = 0x05
	UOPMultiplication     = 0x06
	UOPDivision           = 0x07
	UOPModulo             = 0x08
	UOPExpontiation       = 0x09
	UOPLogicalAnd         = 0x0A
	UOPLogicalOr          = 0x0B
	UOPEqual              = 0x0C
	UOPNotEqual           = 0x0D
	UOPNot                = 0x0E
	UOPNegate             = 0x0F
	UOPBoundsCheck        = 0x10
	UOPGreaterThan        = 0x11
	UOPGreaterThanOrEqual = 0x12
	UOPLessThan           = 0x13
	UOPLessThanOrEqual    = 0x14
	UOPLeftShift          = 0x15
	UOPRightShift         = 0x16
	UOPBitwiseAnd         = 0x17
	UOPBitwordOr          = 0x18
	UOPBitwiseXor         = 0x19
	UOPBitwiseNot         = 0x1A
)

func BinaryOperatorUOP(operator operators.Operator) float64 {
	switch operator {
	case operators.Addition:
		return UOPAddition
	case operators.Subtraction:
		return UOPSubtraction
	case operators.Multiplication:
		return UOPMultiplication
	case operators.Division:
		return UOPDivision
	case operators.Modulo:
		return UOPModulo
	case operators.Exponentiation:
		return UOPExpontiation
	case operators.LogicalAnd:
		return UOPLogicalAnd
	case operators.LogicalOr:
		return UOPLogicalOr
	case operators.Equal:
		return UOPEqual
	case operators.NotEqual:
		return UOPNotEqual
	case operators.BoundsCheck:
		return UOPBoundsCheck
	case operators.GreaterThan:
		return UOPGreaterThan
	case operators.GreaterThanOrEqual:
		return UOPGreaterThanOrEqual
	case operators.LessThan:
		return UOPLessThan
	case operators.LessThanOrEqual:
		return UOPLessThanOrEqual
	case operators.LeftShift:
		return UOPLeftShift
	case operators.RightShift:
		return UOPRightShift
	case operators.BitwiseAnd:
		return UOPBitwiseAnd
	case operators.BitwiseOr:
		return UOPBitwordOr
	case operators.BitwiseXor:
		return UOPBitwiseXor
	default:
		panic(fmt.Errorf("invalid operator %q", operator))
	}
}

func UnaryOperatorUOP(operator operators.Operator) float64 {
	switch operator {
	case operators.Not:
		return UOPNot
	case operators.Negate:
		return UOPNegate
	case operators.BitwiseNot:
		return UOPBitwiseNot
	default:
		panic(fmt.Errorf("invalid operator %q", operator))
	}
}
