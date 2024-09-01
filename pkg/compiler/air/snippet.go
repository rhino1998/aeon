package air

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/operators"
	"github.com/rhino1998/aeon/pkg/compiler/types"
)

type Snippet []Instruction

func (s *Snippet) AllocConst(dst *Operand, size Size) {
	*s = append(*s, Alc{
		Dst:  dst,
		Size: ImmediateOperand(Int(size)),
	})
}

func (s *Snippet) Alloc(dst *Operand, size *Operand) {
	*s = append(*s, Alc{
		Dst:  dst,
		Size: size,
	})
}

func (s *Snippet) App(dst *Value, src *Value, elem *Value) error {
	elemSize, err := TypeSize(elem.Type)
	if err != nil {
		return err
	}
	*s = append(*s, App{
		Dst:  dst.Operand,
		Src:  src.Operand,
		Elem: elem.Operand,
		Size: elemSize,
	})

	return nil
}

func (s Snippet) ResolveLabels() error {
	labelIndices := make(map[Label]int)
	for i, bc := range s {
		if lbc, ok := bc.(LabelledInstruction); ok {
			for _, label := range lbc.Labels {
				labelIndices[label] = i
			}

			s[i] = lbc.Instruction
		}
	}

	return s.walk(bytecodeOperandWalker(func(index int, o *Operand) (*Operand, error) {
		switch o.Kind {
		case OperandKindLabel:
			label := o.Value.(Label)

			labelIndex, ok := labelIndices[label]
			if !ok {
				return nil, fmt.Errorf("unknown label %q in operand", label)
			}

			return ImmediateOperand(Int(labelIndex - index)), nil
		default:
			return o, nil
		}
	}))
}

func (s Snippet) ResolveTypes(typs []types.Type) error {
	typeIndices := make(map[types.Name]int)
	for i, typ := range typs {
		typeIndices[typ.GlobalName()] = i
	}

	return s.walk(bytecodeOperandWalker(func(index int, o *Operand) (*Operand, error) {
		switch o.Kind {
		case OperandKindType:
			index, ok := typeIndices[o.Value.(types.Name)]
			if !ok {
				return nil, fmt.Errorf("unresolvable type %s", o.Value.(TypeName))
			}

			return ImmediateOperand(Int(index)), nil
		default:
			return o, nil
		}
	}))
}

func (s Snippet) ResolveStrings(strings []String) error {
	strIndices := make(map[String]int)
	for i, typ := range strings {
		strIndices[typ] = i
	}

	return s.walk(bytecodeOperandWalker(func(index int, o *Operand) (*Operand, error) {
		switch o.Kind {
		case OperandKindString:
			index, ok := strIndices[o.Value.(String)]
			if !ok {
				return nil, fmt.Errorf("unresolvable string %s", o.Value.(String))
			}

			return ImmediateOperand(Int(index)), nil
		default:
			return o, nil
		}
	}))
}

func (s Snippet) ResolveGlobals(addr Addr) (Size, []types.Type, error) {
	var globals []types.Type
	var totalSize Size = 1

	err := s.walk(bytecodeOperandWalker(func(index int, o *Operand) (*Operand, error) {
		switch o.Kind {
		case OperandKindGlobal:
			local := o.Value.(Variable)
			*o = *ImmediateOperand(Int(addr + Addr(totalSize)))
			globals = append(globals, local.Type())
			globalSize, err := TypeSize(local.Type())
			if err != nil {
				return nil, err
			}

			totalSize += globalSize

			return o, nil
		default:
			return o, nil
		}
	}))

	if err != nil {
		return 0, nil, err
	}

	return totalSize, globals, nil
}

func (s Snippet) ResolveLocals() (Size, []types.Type, error) {
	var locals []types.Type
	var totalSize Size = 1

	err := s.walk(bytecodeOperandWalker(func(index int, o *Operand) (*Operand, error) {
		switch o.Kind {
		case OperandKindLocal:
			local := o.Value.(Variable)
			*o = *OperandRegisterFP.AddConst(totalSize)
			locals = append(locals, local.Type())
			localSize, err := TypeSize(local.Type())
			if err != nil {
				return nil, err
			}

			totalSize += localSize

			return o, nil
		case OperandKindTemp:
			// TODO: maybe register allocate here
			local := o.Value.(Variable)
			*o = *OperandRegisterFP.AddConst(totalSize)
			locals = append(locals, local.Type())
			localSize, err := TypeSize(local.Type())
			if err != nil {
				return nil, err
			}

			totalSize += localSize

			return o, nil
		default:
			return o, nil
		}
	}))

	if err != nil {
		return 0, nil, err
	}

	return totalSize, locals, nil
}

type bytecodeWalker struct {
	Mov   func(int, Mov) (Instruction, error)
	Jmp   func(int, Jmp) (Instruction, error)
	BinOp func(int, BinOp) (Instruction, error)
	UnOp  func(int, UnOp) (Instruction, error)
	Cal   func(int, Cal) (Instruction, error)
	Ret   func(int, Ret) (Instruction, error)
	Alc   func(int, Alc) (Instruction, error)
	App   func(int, App) (Instruction, error)
}

func bytecodeOperandWalker(w func(int, *Operand) (*Operand, error)) bytecodeWalker {
	wi := func(index int) operandWalkFunc {
		return func(o *Operand) (*Operand, error) {
			return w(index, o)
		}
	}

	return bytecodeWalker{
		Mov: func(index int, bc Mov) (Instruction, error) {
			var err error
			bc.Dst, err = bc.Dst.walk(wi(index))
			if err != nil {
				return nil, err
			}

			bc.Src, err = bc.Src.walk(wi(index))
			if err != nil {
				return nil, err
			}

			return bc, nil
		},
		Jmp: func(index int, bc Jmp) (Instruction, error) {
			var err error
			bc.Target, err = bc.Target.walk(wi(index))
			if err != nil {
				return nil, err
			}

			bc.Cond, err = bc.Cond.walk(wi(index))
			if err != nil {
				return nil, err
			}

			return bc, nil
		},
		BinOp: func(index int, bc BinOp) (Instruction, error) {
			var err error
			bc.Dst, err = bc.Dst.walk(wi(index))
			if err != nil {
				return nil, err
			}

			bc.Left, err = bc.Left.walk(wi(index))
			if err != nil {
				return nil, err
			}

			bc.Right, err = bc.Right.walk(wi(index))
			if err != nil {
				return nil, err
			}

			return bc, nil
		},
		UnOp: func(index int, bc UnOp) (Instruction, error) {
			var err error
			bc.Dst, err = bc.Dst.walk(wi(index))
			if err != nil {
				return nil, err
			}

			bc.Src, err = bc.Src.walk(wi(index))
			if err != nil {
				return nil, err
			}

			return bc, nil
		},
		Cal: func(index int, bc Cal) (Instruction, error) {
			var err error
			bc.Func, err = bc.Func.walk(wi(index))
			if err != nil {
				return nil, err
			}

			return bc, nil
		},
		Ret: func(index int, bc Ret) (Instruction, error) {
			return bc, nil
		},
		Alc: func(index int, bc Alc) (Instruction, error) {
			var err error
			bc.Dst, err = bc.Dst.walk(wi(index))
			if err != nil {
				return nil, err
			}

			return bc, nil
		},
		App: func(index int, bc App) (Instruction, error) {
			var err error
			bc.Dst, err = bc.Dst.walk(wi(index))
			if err != nil {
				return nil, err
			}

			bc.Src, err = bc.Src.walk(wi(index))
			if err != nil {
				return nil, err
			}

			bc.Elem, err = bc.Elem.walk(wi(index))
			if err != nil {
				return nil, err
			}

			return bc, nil
		},
	}
}

func (s Snippet) walk(w bytecodeWalker) error {
	var walkBC func(int, Instruction) (Instruction, error)
	walkBC = func(i int, bc Instruction) (Instruction, error) {
		switch bc := bc.(type) {
		case Jmp:
			if w.Jmp == nil {
				return bc, nil
			}
			return w.Jmp(i, bc)
		case Mov:
			if w.Mov == nil {
				return bc, nil
			}
			return w.Mov(i, bc)
		case BinOp:
			if w.BinOp == nil {
				return bc, nil
			}
			return w.BinOp(i, bc)
		case UnOp:
			if w.UnOp == nil {
				return bc, nil
			}
			return w.UnOp(i, bc)
		case Cal:
			if w.Cal == nil {
				return bc, nil
			}
			return w.Cal(i, bc)
		case Ret:
			if w.Ret == nil {
				return bc, nil
			}
			return w.Ret(i, bc)
		case Alc:
			if w.Alc == nil {
				return bc, nil
			}
			return w.Alc(i, bc)
		case App:
			if w.App == nil {
				return bc, nil
			}
			return w.App(i, bc)
		case LabelledInstruction:
			subBC, err := walkBC(i, bc.Instruction)
			if err != nil {
				return nil, err
			}

			bc.Instruction = subBC
			return bc, nil
		default:
			return nil, fmt.Errorf("unhandled bytecode type %T", bc)
		}
	}

	for i, bc := range s {
		var err error
		s[i], err = walkBC(i, bc)
		if err != nil {
			return err
		}
	}

	return nil
}

func (s Snippet) OptimizeOperands() error {
	return s.walk(bytecodeOperandWalker(func(_ int, o *Operand) (*Operand, error) {
		return o.Optimize(), nil
	}))
}

func (s *Snippet) LabelFirst(labels ...Label) {
	s.LabelIndex(0, labels...)
}

func (s *Snippet) LabelLast(labels ...Label) {
	s.LabelIndex(len(*s)-1, labels...)
}

func (s *Snippet) LabelIndex(i int, labels ...Label) {
	if lbc, ok := (*s)[i].(LabelledInstruction); ok {
		lbc.Labels = append(lbc.Labels, labels...)
		(*s)[i] = lbc
	} else {
		(*s)[i] = LabelledInstruction{
			Labels:      labels,
			Instruction: (*s)[i],
		}
	}
}

func (s *Snippet) Mount(rel Relocatable) {
	rel.OffsetAddr(Addr(len(*s)))
	s.Add(rel.Instructions()...)
}

func (s *Snippet) Add(bcs ...Instruction) {
	*s = append(*s, bcs...)
}

func (s *Snippet) JumpTo(label Label, cond *Value) {
	*s = append(*s, Jmp{
		Cond:   cond.Operand,
		Target: OperandRegisterPC.Add(LabelOperand(label)).AddConst(-1),
	})
}

func (s *Snippet) JumpAfter(label Label, cond *Value) {
	*s = append(*s, Jmp{
		Cond:   cond.Operand,
		Target: OperandRegisterPC.Add(LabelOperand(label)),
	})
}

func (s *Snippet) BinOp(dst *Value, left *Value, operator operators.Operator, kind kinds.Kind, right *Value) {
	s.Add(BinOp{
		Op:    operator,
		Kind:  kind,
		Dst:   dst.Operand,
		Left:  left.Operand,
		Right: right.Operand,
		Size:  1,
	})
}

func (s *Snippet) Mov(dst, src *Value) error {
	dstSize, err := TypeSize(dst.Type)
	if err != nil {
		return err
	}

	srcSize, err := TypeSize(src.Type)
	if err != nil {
		return err
	}

	if dstSize != srcSize {
		return fmt.Errorf("cannot move %s to %s", src.Type, dst.Type)
	}

	s.Add(Mov{
		Dst:  dst.Operand,
		Src:  src.Operand,
		Size: dstSize,
	})

	return nil
}

func (s *Snippet) Ret(size Size) {
	s.Add(Ret{
		Args: size,
	})
}
