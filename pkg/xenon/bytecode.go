package xenon

import (
	"fmt"
	"strings"

	"github.com/rhino1998/aeon/pkg/compiler"
)

type ValueSource int

const (
	ValueSourceImmediate ValueSource = 0
	ValueSourceRegister  ValueSource = 1
	ValueSourceMemory    ValueSource = 2
	ValueSourceLocal     ValueSource = 3
)

func (s ValueSource) MarshalXenon() ([]byte, error) {
	switch s {
	case ValueSourceImmediate:
		return []byte("I"), nil
	case ValueSourceMemory:
		return []byte("M"), nil
	case ValueSourceRegister:
		return []byte("R"), nil
	case ValueSourceLocal:
		return []byte("L"), nil
	default:
		return nil, fmt.Errorf("invalid value source %x", s)
	}
}

const FrameSize = 3

type Bytecode interface {
	xenon() string
}

type Int int64

func (Int) immediate() {}

func (i Int) String() string {
	return fmt.Sprintf("Int(%d)", int64(i))
}

type Float float64

func (Float) immediate() {}

type String string

func (String) immediate() {}

func (s String) String() string {
	return fmt.Sprintf("String(%v)", string(s))
}

type Bool bool

func (Bool) immediate() {}

func (b Bool) String() string {
	return fmt.Sprintf("Bool(%v)", bool(b))
}

type Register int64

func (r Register) String() string {
	return fmt.Sprintf("reg%02x", int64(r))
}

type Addr uint64

func (a Addr) String() string {
	return fmt.Sprintf("0x%08x", uint64(a))
}

func (a Addr) Offset(o AddrOffset) Addr {
	return Addr(int64(a) + int64(o))
}

type AddrOffset int64

func (o AddrOffset) String() string {
	if o < 0 {
		return fmt.Sprintf("-0x%04x", int64(-o))
	} else {
		return fmt.Sprintf("+0x%04x", int64(o))
	}
}

type Immediate interface {
	immediate()
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

func (Nop) xenon() string {
	return "nop"
}

type Mov struct {
	Src Operand `xc:"s"`
	Dst Operand `xc:"d"`
}

func (m Mov) xenon() string {
	return "mov"
}

func (m Mov) String() string {
	return fmt.Sprintf("MOV %v = %v", m.Dst, m.Src)
}

type Store struct {
	Src Operand `xc:"s"`
	Dst Operand `xc:"d"`
}

func (s Store) String() string {
	return fmt.Sprintf("Store *%v = %v", s.Dst, s.Src)
}

func (s Store) xenon() string {
	return "store"
}

type Load struct {
	Src Operand `xc:"s"`
	Dst Operand `xc:"d"`
}

func (m Load) String() string {
	return fmt.Sprintf("Load %v = *%v", m.Dst, m.Src)
}

func (m Load) xenon() string {
	return "load"
}

type Push struct {
	Src Operand `xc:"s"`
}

func (p Push) String() string {
	return fmt.Sprintf("PUSH %v", p.Src)
}

func (p Push) xenon() string {
	return "push"
}

type Convert[To, From any] struct {
	Register
}

type ConvertIntFloat = Convert[int64, float64]
type ConvertFloatInt = Convert[float64, int64]

type Operand struct {
	Source ValueSource `xc:"s"`
	Value  any         `xc:"v"`
}

func (o Operand) String() string {
	return fmt.Sprintf("%s", o.Value)
}

func ImmediateOperand(imm Immediate) Operand {
	return Operand{
		Value:  imm,
		Source: ValueSourceImmediate,
	}
}

type BinOp struct {
	Op    Operator `xc:"o"`
	Dst   Operand  `xc:"d"`
	Left  Operand  `xc:"l"`
	Right Operand  `xc:"r"`
}

func (o BinOp) xenon() string {
	return "bop"
}

func (o BinOp) String() string {
	return fmt.Sprintf("BinOp(%s) %v = %v %v %v", o.Op, o.Dst, o.Left, o.Op, o.Right)
}

type Return struct{}

func (r Return) xenon() string {
	return "ret"
}

func (r Return) String() string {
	return fmt.Sprintf("RET")
}

type CallExtern struct {
	Args   int
	Extern string
}

func (e CallExtern) xenon() string {
	return "ext"
}

type Call[Func any] struct {
	Func Func `xc:"f"`
}

func (c Call[Func]) xenon() string {
	return "call"
}

type CallAddr = Call[Addr]
type CallClosure = Call[Closure]

func shortKind(k compiler.Kind) string {
	switch k {
	case compiler.KindBool:
		return "B"
	case compiler.KindInt:
		return "I"
	case compiler.KindFloat:
		return "F"
	case compiler.KindString:
		return "S"
	case compiler.KindPointer:
		return "*"
	case compiler.KindTuple:
		return "T"
	case compiler.KindSlice:
		return "["
	case compiler.KindMap:
		return "M"
	case compiler.KindStruct:
		return "X"
	default:
		return "U"
	}
}

type Operator string

func BinaryOperator(left compiler.Kind, op compiler.Operator, right compiler.Kind) Operator {
	return Operator(fmt.Sprintf("%s%s%s", shortKind(left), string(op), shortKind(right)))
}

type Jmp struct {
	Dst Operand `xc:"d"`
}

func (j Jmp) xenon() string {
	return "jmp"
}

func (j Jmp) String() string {
	return fmt.Sprintf("JMP %v", j.Dst)
}

type JmpR struct {
	Dst Operand `xc:"d"`
}

func (j JmpR) xenon() string {
	return "jmpr"
}

func (j JmpR) String() string {
	return fmt.Sprintf("JMPR %v", j.Dst)
}

type JmpRC struct {
	Invert bool    `xc:"i"`
	Src    Operand `xc:"s"`
	Dst    Operand `xc:"d"`
}

func (j JmpRC) xenon() string {
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
	Kind string  `xc:"k"`
	Dst  Operand `xc:"d"`
	Size Operand `xc:"s"`
}

func (Make) xenon() string {
	return "make"
}

func (m Make) String() string {
	return fmt.Sprintf("MAKE(%s) %v %d", m.Kind, m.Dst, m.Size)
}

type Index struct {
	Kind  string  `xc:"k"`
	Base  Operand `xc:"b"`
	Index Operand `xc:"i"`
	Dst   Operand `xc:"d"`
}

func (Index) xenon() string {
	return "ind"
}

type SetIndex struct {
	Kind  string  `xc:"k"`
	Base  Operand `xc:"b"`
	Index Operand `xc:"i"`
	Src   Operand `xc:"s"`
}

func (SetIndex) xenon() string {
	return "set"
}
