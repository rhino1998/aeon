package abc

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler/air"
	"github.com/rhino1998/aeon/pkg/compiler/operators"
)

type Snippet []float64

func (s *Snippet) Add(f ...float64) {
	*s = append(*s, f...)
}

const (
	Nop float64 = iota
	Mov
	Jmp
	Binop
	Unop
	Call
	Ret
	Alc
	App
)

func Compile(instructions []air.Instruction) Snippet {
	var bc Snippet

	var offset int
	labels := make(map[air.Label]int)
	for _, instruction := range instructions {
		switch instruction := instruction.(type) {
		case air.LabelledInstruction:
			for _, label := range instruction.Labels {
				if _, ok := labels[label]; ok {
					panic(fmt.Sprintf("bug: duplicate label %s", label))
				}
				labels[label] = offset
			}
		}

		offset += InstructionSize(instruction)
	}

	for _, instruction := range instructions {
		bc = append(bc, compileInstruction(instruction, len(bc), labels)...)
	}

	return bc
}

func compileInstruction(instruction air.Instruction, current int, labels map[air.Label]int) Snippet {
	var s Snippet
	switch ins := instruction.(type) {
	case air.LabelledInstruction:
		return compileInstruction(ins.Instruction, current, labels)
	case air.Nop:
		s.Add(Nop)
	case air.Mov:
		s.Add(Mov)
		s.Add(float64(ins.Size))
		dst := compileOperand(ins, ins.Dst, 0, labels)
		s.Add(float64(len(dst)))
		s.Add(dst...)
		src := compileOperand(ins, ins.Src, 0, labels)
		s.Add(float64(len(src)))
		s.Add(src...)
	case air.Jmp:
		s.Add(Jmp)
		target := compileOperand(ins, ins.Target, current, labels)
		s.Add(float64(len(target)))
		s.Add(target...)
		cond := compileOperand(ins, ins.Cond, current, labels)
		s.Add(float64(len(cond)))
		s.Add(cond...)
	case air.BinOp:
		s.Add(Binop)
		dst := compileOperand(ins, ins.Dst, 0, labels)
		s.Add(float64(len(dst)))
		s.Add(dst...)
		s.Add(float64(ins.Kind))
		s.Add(float64(BinaryOperatorUOP(ins.Op)))
		left := compileOperand(ins, ins.Left, 0, labels)
		s.Add(float64(len(left)))
		s.Add(left...)
		right := compileOperand(ins, ins.Right, 0, labels)
		s.Add(float64(len(right)))
		s.Add(right...)
	case air.UnOp:
		s.Add(Unop)
		dst := compileOperand(ins, ins.Dst, 0, labels)
		s.Add(float64(len(dst)))
		s.Add(dst...)
		s.Add(float64(ins.Kind))
		s.Add(float64(UnaryOperatorUOP(ins.Op)))
		src := compileOperand(ins, ins.Src, 0, labels)
		s.Add(float64(len(src)))
		s.Add(src...)
	case air.Cal:
		s.Add(Call)
		s.Add(float64(ins.Line))
		fun := compileOperand(ins, ins.Func, 0, labels)
		s.Add(float64(len(fun)))
		s.Add(fun...)
	case air.Ret:
		s.Add(Ret)
		s.Add(float64(ins.Args))
	case air.Alc:
		s.Add(Alc)
		dst := compileOperand(ins, ins.Dst, 0, labels)
		s.Add(float64(len(dst)))
		s.Add(dst...)
		size := compileOperand(ins, ins.Size, 0, labels)
		s.Add(float64(len(size)))
		s.Add(size...)
	case air.App:
		s.Add(App)
		dst := compileOperand(ins, ins.Dst, 0, labels)
		s.Add(float64(len(dst)))
		s.Add(dst...)
		src := compileOperand(ins, ins.Src, 0, labels)
		s.Add(float64(len(src)))
		s.Add(src...)
		elem := compileOperand(ins, ins.Elem, 0, labels)
		s.Add(float64(len(elem)))
		s.Add(elem...)
		s.Add(float64(ins.Size))
	default:
		panic(fmt.Errorf("invalid instruction type %T", ins))
	}

	return s
}

func compileOperand(ins air.Instruction, operand *air.Operand, current int, labels map[air.Label]int) Snippet {
	var s Snippet
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
		s.Add(UOPRegister, float64(operand.Value.(air.Register)))
	case air.OperandKindIndirect:
		s.Add(compileOperand(ins, operand.Value.(air.Indirect).Ptr, current, labels)...)
		s.Add(UOPIndirect)
	case air.OperandKindBinary:
		s.Add(compileOperand(ins, operand.Value.(air.BinaryOperand).Left, current, labels)...)
		s.Add(compileOperand(ins, operand.Value.(air.BinaryOperand).Right, current, labels)...)
		s.Add(BinaryOperatorUOP(operand.Value.(air.BinaryOperand).Op))
	case air.OperandKindUnary:
		s.Add(compileOperand(ins, operand.Value.(air.UnaryOperand).A, current, labels)...)
		s.Add(UnaryOperatorUOP(operand.Value.(air.UnaryOperand).Op))
	case air.OperandKindVTableLookup:
		s.Add(compileOperand(ins, operand.Value.(air.VTableLookup).Type, current, labels)...)
		s.Add(compileOperand(ins, operand.Value.(air.VTableLookup).Method, current, labels)...)
		s.Add(UOPVTableLookup)
	case air.OperandKindLabel:
		label, ok := labels[operand.Value.(air.Label)]
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
		return 2
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
	UOPImmediate float64 = iota
	UOPRegister
	UOPIndirect
	UOPVTableLookup
	UOPAddition
	UOPSubtraction
	UOPMultiplication
	UOPDivision
	UOPModulo
	UOPLogicalAnd
	UOPLogicalOr
	UOPEqual
	UOPNotEqual
	UOPNot
	UOPNegate
	UOPBoundsCheck
	UOPGreaterThan
	UOPGreaterThanOrEqual
	UOPLessThan
	UOPLessThanOrEqual
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
	default:
		panic(fmt.Errorf("invalid operator %q", operator))
	}
}
