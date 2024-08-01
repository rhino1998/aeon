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
	ValueSourceAddress   ValueSource = 2
	ValueSourceOffset    ValueSource = 3
)

const FrameSize = 3

type Bytecode interface{}

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

type Nop struct{}

func (Nop) String() string {
	return "NOP"
}

type Mov struct {
	Src Operand
	Dst Operand
}

func (m Mov) String() string {
	return fmt.Sprintf("MOV %v = %v", m.Dst, m.Src)
}

type Store[Src any] struct {
	Src Src      `xc:"s"`
	Dst Register `xc:"d"`
}
type StoreI = Store[Immediate]
type StoreR = Store[Register]

type Load[Dst any] struct {
	Src Register `xc:"s"`
	Dst Dst      `xc:"d"`
}

type LoadR = Load[Register]
type LoadO = Load[AddrOffset]

type Push[Src any] struct {
	Src Src `xc:"s"`
}

func (p Push[Src]) String() string {
	return fmt.Sprintf("PUSH %v", p.Src)
}

type PushR = Push[Register]
type PushM = Push[Addr]
type PushO = Push[AddrOffset]
type PushI = Push[Immediate]

type Convert[To, From any] struct {
	Register
}

type ConvertIntFloat = Convert[int64, float64]
type ConvertFloatInt = Convert[float64, int64]

type Add[T Int | Float | String, Dst, A, B any] struct {
	A   A
	B   B
	Dst Dst
}

func (o Add[T, Dst, A, B]) String() string {
	var t T
	return fmt.Sprintf("ADD(%T) %v + %v = %v", t, o.A, o.B, o.Dst)
}

type Operand struct {
	Kind   compiler.Kind
	Source ValueSource
	Value  any
}

func (o Operand) String() string {
	return fmt.Sprintf("%s(%s)", o.Kind, o.Value)
}

type BinOp struct {
	Op    compiler.Operator
	Dst   Operand
	Left  Operand
	Right Operand
}

func (o BinOp) String() string {
	return fmt.Sprintf("BinOp(%s) %v = %v %v %v", o.Op, o.Dst, o.Left, o.Op, o.Right)
}

type Return struct {
	Register Register `xc:"r"`
}
type CallExtern struct {
	Args   int
	Extern string
}
type Call[Func any] struct {
	Func Func `xc:"f"`
}

type CallAddr = Call[Addr]
type CallClosure = Call[Closure]

type Jump[Dst any] struct {
	Dst Dst `xc:"d"`
}

func (j Jump[Dst]) String() string {
	return fmt.Sprintf("JUMP %v", j.Dst)
}

type JumpA = Jump[Addr]
type JumpO = Jump[AddrOffset]
type JumpR = Jump[Register]

type Make[T, Dst any] struct {
	Dst  Dst
	Size int
}

func (m Make[T, Dst]) Striung() string {
	var t T
	return fmt.Sprintf("MAKE(%T) %v %d", t, m.Dst, m.Size)
}

type MakeTupleR = Make[Tuple, Register]

type Index[T, Base, Index, Dst any] struct {
	Base  Base
	Index Index
	Dst   Dst
}

type IndexTupleRRR = Index[Tuple, Register, Register, Register]
type IndexTupleRIR = Index[Tuple, Register, Register, Register]

type SetIndex[T, Base, Index, Src any] struct {
	Base  Base
	Index Index
	Src   Src
}

type SetIndexTupleRRR = SetIndex[Tuple, Register, Register, Register]
type SetIndexTupleRIR = SetIndex[Tuple, Register, Immediate, Register]
