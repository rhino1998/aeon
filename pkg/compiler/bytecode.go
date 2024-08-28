package compiler

import (
	"fmt"
	"strings"
)

type Bytecode interface {
	Name() string
}

type Relocatable interface {
	OffsetAddr(Addr)
	Bytecode() BytecodeSnippet
}

type LabelledBytecode struct {
	Labels []Label
	Bytecode
}

type BytecodeSnippet []Bytecode

type Label string

func (l Label) String() string {
	return fmt.Sprintf("<%s>", string(l))
}

var labelIndex int64

func newLabel() Label {
	labelIndex++
	return Label(fmt.Sprintf("%d", labelIndex))
}

func (s *BytecodeSnippet) AllocConst(dst *Location, size Size) {
	*s = append(*s, Alc{
		Dst:  dst.Operand,
		Size: ImmediateOperand(Int(size)),
	})
}

func (s *BytecodeSnippet) Alloc(dst *Location, size *Location) {
	*s = append(*s, Alc{
		Dst:  dst.Operand,
		Size: size.Operand,
	})
}

func (s *BytecodeSnippet) App(dst *Location, src *Location, elem *Location, size Size) {
	*s = append(*s, App{
		Dst:  dst.Operand,
		Src:  src.Operand,
		Elem: elem.Operand,
		Size: size,
	})
}

func (s *BytecodeSnippet) Str(dst *Location, str String) {
	*s = append(*s, Str{
		Dst: dst.Operand,
		Str: str,
	})
}

func (s BytecodeSnippet) ResolveLabels() error {
	labelIndices := make(map[Label]int)
	for i, bc := range s {
		if lbc, ok := bc.(LabelledBytecode); ok {
			for _, label := range lbc.Labels {
				labelIndices[label] = i
			}

			s[i] = lbc.Bytecode
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

func (s BytecodeSnippet) ResolveTypes(types []Type) error {
	typeIndices := make(map[TypeName]int)
	for i, typ := range types {
		typeIndices[typ.GlobalName()] = i
	}

	return s.walk(bytecodeOperandWalker(func(index int, o *Operand) (*Operand, error) {
		switch o.Kind {
		case OperandKindType:
			index, ok := typeIndices[o.Value.(TypeName)]
			if !ok {
				return nil, fmt.Errorf("unresolvable type %s", o.Value.(TypeName))
			}

			return ImmediateOperand(Int(index)), nil
		default:
			return o, nil
		}
	}))
}

func (s BytecodeSnippet) ResolveStrings(strings []String) error {
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

func (s BytecodeSnippet) ResolveLocals() (Size, error) {
	var totalSize Size = 1

	err := s.walk(bytecodeOperandWalker(func(index int, o *Operand) (*Operand, error) {
		switch o.Kind {
		case OperandKindLocal:
			local := o.Value.(*Variable)
			*o = *OperandRegisterFP.ConstOffset(totalSize)
			totalSize += local.Type().Size()

			return o, nil
		default:
			return o, nil
		}
	}))

	if err != nil {
		return 0, err
	}

	return totalSize, nil
}

type bytecodeWalker struct {
	Mov   func(int, Mov) (Bytecode, error)
	Jmp   func(int, Jmp) (Bytecode, error)
	BinOp func(int, BinOp) (Bytecode, error)
	UnOp  func(int, UnOp) (Bytecode, error)
	Str   func(int, Str) (Bytecode, error)
	Cal   func(int, Cal) (Bytecode, error)
	Ret   func(int, Ret) (Bytecode, error)
	Alc   func(int, Alc) (Bytecode, error)
	App   func(int, App) (Bytecode, error)
}

func bytecodeOperandWalker(w func(int, *Operand) (*Operand, error)) bytecodeWalker {
	wi := func(index int) operandWalkFunc {
		return func(o *Operand) (*Operand, error) {
			return w(index, o)
		}
	}

	return bytecodeWalker{
		Mov: func(index int, bc Mov) (Bytecode, error) {
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
		Jmp: func(index int, bc Jmp) (Bytecode, error) {
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
		BinOp: func(index int, bc BinOp) (Bytecode, error) {
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
		UnOp: func(index int, bc UnOp) (Bytecode, error) {
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
		Str: func(index int, bc Str) (Bytecode, error) {
			var err error
			bc.Dst, err = bc.Dst.walk(wi(index))
			if err != nil {
				return nil, err
			}

			return bc, nil
		},
		Cal: func(index int, bc Cal) (Bytecode, error) {
			var err error
			bc.Func, err = bc.Func.walk(wi(index))
			if err != nil {
				return nil, err
			}

			return bc, nil
		},
		Ret: func(index int, bc Ret) (Bytecode, error) {
			return bc, nil
		},
		Alc: func(index int, bc Alc) (Bytecode, error) {
			var err error
			bc.Dst, err = bc.Dst.walk(wi(index))
			if err != nil {
				return nil, err
			}

			return bc, nil
		},
		App: func(index int, bc App) (Bytecode, error) {
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

func (s BytecodeSnippet) walk(w bytecodeWalker) error {
	var walkBC func(int, Bytecode) (Bytecode, error)
	walkBC = func(i int, bc Bytecode) (Bytecode, error) {
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
		case Str:
			if w.Str == nil {
				return bc, nil
			}
			return w.Str(i, bc)
		case Cal:
			if w.Str == nil {
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
		case LabelledBytecode:
			subBC, err := walkBC(i, bc.Bytecode)
			if err != nil {
				return nil, err
			}

			bc.Bytecode = subBC
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

func (s BytecodeSnippet) OptimizeOperands() error {
	return s.walk(bytecodeOperandWalker(func(_ int, o *Operand) (*Operand, error) {
		return o.Optimize(), nil
	}))
}

func (s *BytecodeSnippet) LabelFirst(labels ...Label) {
	s.LabelIndex(0, labels...)
}

func (s *BytecodeSnippet) LabelLast(labels ...Label) {
	s.LabelIndex(len(*s)-1, labels...)
}

func (s *BytecodeSnippet) LabelIndex(i int, labels ...Label) {
	if lbc, ok := (*s)[i].(LabelledBytecode); ok {
		lbc.Labels = append(lbc.Labels, labels...)
		(*s)[i] = lbc
	} else {
		(*s)[i] = LabelledBytecode{
			Labels:   labels,
			Bytecode: (*s)[i],
		}
	}
}

func (s *BytecodeSnippet) Mount(rel Relocatable) {
	rel.OffsetAddr(Addr(len(*s)))
	s.Add(rel.Bytecode()...)
}

func (s *BytecodeSnippet) Add(bcs ...Bytecode) {
	*s = append(*s, bcs...)
}

func (s *BytecodeSnippet) JumpTo(label Label, cond *Operand) {
	if cond == nil {
		*s = append(*s, Jmp{
			Cond:   ImmediateOperand(Bool(true)),
			Target: OperandRegisterPC.Offset(LabelOperand(label)).ConstOffset(-1),
		})
	} else {
		*s = append(*s, Jmp{
			Cond:   cond,
			Target: OperandRegisterPC.Offset(LabelOperand(label)).ConstOffset(-1),
		})
	}
}

func (s *BytecodeSnippet) JumpAfter(label Label, cond *Operand) {
	if cond == nil {
		*s = append(*s, Jmp{
			Cond:   ImmediateOperand(Bool(true)),
			Target: OperandRegisterPC.Offset(LabelOperand(label)),
		})
	} else {
		*s = append(*s, Jmp{
			Cond:   cond,
			Target: OperandRegisterPC.Offset(LabelOperand(label)),
		})

	}
}

func (s *BytecodeSnippet) BinOp(dst *Location, left *Location, operator Operator, right *Location) {
	s.Add(BinOp{
		Op:    operator,
		Kind:  left.Type.Kind(),
		Dst:   dst.Operand,
		Left:  left.Operand,
		Right: right.Operand,
		Size:  1,
	})
}

func (s *BytecodeSnippet) Mov(dst, src *Location) {
	s.Add(Mov{
		Dst:  dst.Operand,
		Src:  src.Operand,
		Size: min(dst.Type.Size(), src.Type.Size()),
	})
}

type Int int64

func (Int) immediate() {}

func (i Int) Location(vs *ValueScope) *Location {
	return vs.newImmediate(i)
}

func (Int) Kind() Kind { return KindInt }

func (i Int) String() string {
	return fmt.Sprintf("Int(%d)", int64(i))
}

type Float float64

func (Float) immediate() {}

func (f Float) Location(vs *ValueScope) *Location {
	return vs.newImmediate(f)
}

func (Float) Kind() Kind { return KindFloat }

type String string

func (s String) Location(vs *ValueScope) *Location {
	return vs.getString(s)
}

func (String) Kind() Kind { return KindString }

func (s String) String() string {
	return fmt.Sprintf("String(%v)", string(s))
}

type Bool bool

func (Bool) immediate() {}

func (b Bool) Location(vs *ValueScope) *Location {
	return vs.newImmediate(b)
}

func (Bool) Kind() Kind { return KindBool }

func (b Bool) String() string {
	return fmt.Sprintf("Bool(%v)", bool(b))
}

type Addr uint64

func (Addr) immediate() {}

func (Addr) Kind() Kind { return KindPointer }

func (a Addr) String() string {
	return fmt.Sprintf("0x%08x", uint64(a))
}

func (a Addr) Offset(o Size) Addr {
	return Addr(int64(a) + int64(o))
}

type Size int64

func (o Size) String() string {
	if o < 0 {
		return fmt.Sprintf("-0x%02x", int64(-o))
	} else {
		return fmt.Sprintf("+0x%02x", int64(o))
	}
}

type Immediate interface {
	immediate()
	Kind() Kind
}

type Struct map[string]any

type Tuple []any

func (t Tuple) String() string {
	var parts []string
	for _, e := range t {
		parts = append(parts, fmt.Sprintf("%v", e))
	}

	return fmt.Sprintf("Tuple(%s)", strings.Join(parts, ", "))
}

type Slice []any

type Closure struct {
	Addr      Addr
	EnvStart  Addr
	EnvLength int
}

type Map map[any]any

func (m Map) String() string {
	return fmt.Sprintf("Map(%v)", map[any]any(m))
}

type Nop struct{}

func (Nop) String() string {
	return "NOP"
}

func (Nop) Name() string {
	return "nop"
}

type Mov struct {
	Src  *Operand `xc:"s"`
	Dst  *Operand `xc:"d"`
	Size Size     `xc:"c"`
}

func (m Mov) Name() string {
	return "mov"
}

func (m Mov) String() string {
	return fmt.Sprintf("MOV(%v) %v = %v", m.Size, m.Dst, m.Src)
}

type Store struct {
	Src *Operand `xc:"s"`
	Dst *Operand `xc:"d"`
}

func (s Store) String() string {
	return fmt.Sprintf("Store *%v = %v", s.Dst, s.Src)
}

func (s Store) xenon() string {
	return "store"
}

type Load struct {
	Src *Operand `xc:"s"`
	Dst *Operand `xc:"d"`
}

func (m Load) String() string {
	return fmt.Sprintf("Load %v = *%v", m.Dst, m.Src)
}

func (m Load) xenon() string {
	return "load"
}

type Convert[To, From any] struct {
	Register
}

type ConvertIntFloat = Convert[int64, float64]
type ConvertFloatInt = Convert[float64, int64]

type UnOp struct {
	Op  Operation `xc:"o"`
	Dst *Operand  `xc:"d"`
	Src *Operand  `xc:"s"`
}

func (o UnOp) Name() string {
	return "uop"
}

func (o UnOp) String() string {
	return fmt.Sprintf("UnOp(%s) %v = %v %v", o.Op, o.Dst, o.Op, o.Src)
}

type BinOp struct {
	Op    Operator
	Kind  Kind
	Dst   *Operand
	Left  *Operand
	Right *Operand
	Size  Size
}

func (o BinOp) Name() string {
	return "bop"
}

func (o BinOp) String() string {
	return fmt.Sprintf("BinOp(%s) %v = %v %v %v", o.Size, o.Dst, o.Left, o.Op, o.Right)
}

type Ret struct {
	Args Size `xc:"s"`
}

func (r Ret) Name() string {
	return "ret"
}

func (r Ret) String() string {
	return fmt.Sprintf("RET(%s)", r.Args)
}

type Cal struct {
	Func *Operand `xc:"f"`
}

func (c Cal) String() string {
	return fmt.Sprintf("CAL %s", c.Func)
}

func (c Cal) Name() string {
	return "cal"
}

func shortKind(k Kind) string {
	switch k {
	case KindBool:
		return "B"
	case KindInt:
		return "I"
	case KindFloat:
		return "F"
	case KindString:
		return "S"
	case KindPointer:
		return "P"
	case KindTuple:
		return "("
	case KindSlice:
		return "["
	case KindMap:
		return "M"
	case KindStruct:
		return "X"
	case KindType:
		return "T"
	default:
		return "U"
	}
}

type Jmp struct {
	Cond   *Operand `xc:"s"`
	Target *Operand `xc:"d"`
}

func (j Jmp) Name() string {
	return "jmp"
}

func (j Jmp) String() string {
	return fmt.Sprintf("JMP %v %v", j.Target, j.Cond)
}

type Str struct {
	Dst *Operand
	Str String
}

func (s Str) Name() string {
	return "str"
}

func (s Str) String() string {
	return fmt.Sprintf("STR %v = %q", s.Dst, s.Str)
}

type Alc struct {
	Dst  *Operand
	Size *Operand
}

func (a Alc) Name() string {
	return "alc"
}

func (a Alc) String() string {
	return fmt.Sprintf("ALC(%v) %v", a.Size, a.Dst)
}

type App struct {
	Dst  *Operand
	Src  *Operand
	Elem *Operand
	Size Size
}

func (a App) Name() string {
	return "app"
}

func (a App) String() string {
	return fmt.Sprintf("APP(%v) %v = %v + [%v]", a.Size, a.Dst, a.Src, a.Elem)
}
