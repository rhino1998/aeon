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

type BytecodeSnippet []Bytecode

func (s *BytecodeSnippet) Mount(rel Relocatable) {
	rel.SetAddr(Addr(len(*s)))
	s.Add(rel.Bytecode()...)
}

func (s *BytecodeSnippet) Add(bcs ...Bytecode) {
	*s = append(*s, bcs...)
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

func (Int) Kind() Kind { return KindInt }

func (i Int) String() string {
	return fmt.Sprintf("Int(%d)", int64(i))
}

type Float float64

func (Float) immediate() {}

func (Float) Kind() Kind { return KindFloat }

type String string

func (String) immediate() {}

func (String) Kind() Kind { return KindString }

func (s String) String() string {
	return fmt.Sprintf("String(%v)", string(s))
}

type Bool bool

func (Bool) immediate() {}

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
	Args Size     `xc:"a"`
	Func *Operand `xc:"f"`
}

func (c Call) String() string {
	return fmt.Sprintf("CALL(%s) %s", c.Args, c.Func)
}

func (c Call) Name() string {
	return "call"
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
	Dst *Operand `xc:"d"`
}

func (j Jmp) Name() string {
	return "jmp"
}

func (j Jmp) String() string {
	return fmt.Sprintf("JMP %v", j.Dst)
}

type JmpR struct {
	Dst *Operand `xc:"d"`
}

func (j JmpR) Name() string {
	return "jmpr"
}

func (j JmpR) String() string {
	return fmt.Sprintf("JMPR %v", j.Dst)
}

type JmpRC struct {
	Invert bool     `xc:"i"`
	Src    *Operand `xc:"s"`
	Dst    *Operand `xc:"d"`
}

func (j JmpRC) Name() string {
	return "jmprc"
}

func (j JmpRC) String() string {
	not := ""
	if j.Invert {
		not = "not "
	}
	return fmt.Sprintf("JMPRC %v if %s%v", j.Dst, not, j.Src)
}

type Make struct {
	Kind string   `xc:"k"`
	Dst  *Operand `xc:"d"`
	Size *Operand `xc:"s"`
}

func (Make) Name() string {
	return "make"
}

func (m Make) String() string {
	return fmt.Sprintf("MAKE(%s) %v %d", m.Kind, m.Dst, m.Size)
}

type Index struct {
	Kind  string   `xc:"k"`
	Base  *Operand `xc:"b"`
	Index *Operand `xc:"i"`
	Dst   *Operand `xc:"d"`
}

func (Index) xenon() string {
	return "ind"
}

type SetIndex struct {
	Kind  string   `xc:"k"`
	Base  *Operand `xc:"b"`
	Index *Operand `xc:"i"`
	Src   *Operand `xc:"s"`
}

func (SetIndex) xenon() string {
	return "set"
}
