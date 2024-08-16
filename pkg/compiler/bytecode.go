package compiler

import (
	"fmt"
	"strings"
)

type Bytecode interface {
	Name() string
}

type Relocatable interface {
	SetAddr(Addr)
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

			// TODO: maybe off by one
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

type bytecodeWalker struct {
	Mov    func(int, Mov) (Bytecode, error)
	Jmp    func(int, Jmp) (Bytecode, error)
	BinOp  func(int, BinOp) (Bytecode, error)
	UnOp   func(int, UnOp) (Bytecode, error)
	Str    func(int, Str) (Bytecode, error)
	Call   func(int, Call) (Bytecode, error)
	Return func(int, Return) (Bytecode, error)
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
		Call: func(index int, bc Call) (Bytecode, error) {
			var err error
			bc.Func, err = bc.Func.walk(wi(index))
			if err != nil {
				return nil, err
			}

			return bc, nil
		},
		Return: func(index int, bc Return) (Bytecode, error) {
			return bc, nil
		},
	}
}

func (s BytecodeSnippet) walk(w bytecodeWalker) error {
	for i, bc := range s {
		var err error
		switch bc := bc.(type) {
		case Jmp:
			if w.Jmp == nil {
				continue
			}
			s[i], err = w.Jmp(i, bc)
		case Mov:
			if w.Mov == nil {
				continue
			}
			s[i], err = w.Mov(i, bc)
		case BinOp:
			if w.BinOp == nil {
				continue
			}
			s[i], err = w.BinOp(i, bc)
		case UnOp:
			if w.UnOp == nil {
				continue
			}
			s[i], err = w.UnOp(i, bc)
		case Str:
			if w.Str == nil {
				continue
			}
			s[i], err = w.Str(i, bc)
		case Call:
			if w.Str == nil {
				continue
			}
			s[i], err = w.Call(i, bc)
		case Return:
			if w.Str == nil {
				continue
			}
			s[i], err = w.Return(i, bc)
		default:
			return fmt.Errorf("unhandled bytecode type %T", bc)
		}

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
	rel.SetAddr(Addr(len(*s)))
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
		Op:    BinaryOperation(left.Type.Kind(), operator, right.Type.Kind()),
		Dst:   dst.Operand,
		Left:  left.Operand,
		Right: right.Operand,
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
	return fmt.Sprintf("unOp(%s) %v = %v %v", o.Op, o.Dst, o.Op, o.Src)
}

type BinOp struct {
	Op    Operation `xc:"o"`
	Dst   *Operand  `xc:"d"`
	Left  *Operand  `xc:"l"`
	Right *Operand  `xc:"r"`
}

func (o BinOp) Name() string {
	return "bop"
}

func (o BinOp) String() string {
	return fmt.Sprintf("BinOp(%s) %v = %v %v %v", o.Op, o.Dst, o.Left, o.Op, o.Right)
}

type Return struct {
	Args Size `xc:"s"`
}

func (r Return) Name() string {
	return "ret"
}

func (r Return) String() string {
	return fmt.Sprintf("RET(%s)", r.Args)
}

type Call struct {
	Func *Operand `xc:"f"`
}

func (c Call) String() string {
	return fmt.Sprintf("CAL %s", c.Func)
}

func (c Call) Name() string {
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
		return "T"
	case KindSlice:
		return "["
	case KindMap:
		return "M"
	case KindStruct:
		return "X"
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
