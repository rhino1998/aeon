package air

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/operators"
	"github.com/rhino1998/aeon/pkg/compiler/types"
)

type Instruction interface {
	Name() string
}

type Relocatable interface {
	OffsetAddr(Addr)
	Instructions() Snippet
}

type LabelledInstruction struct {
	Labels []Label
	Instruction
}

type Label string

func (l Label) String() string {
	return fmt.Sprintf("<%s>", string(l))
}

var labelIndex int64

func NewLabel() Label {
	labelIndex++
	return Label(fmt.Sprintf("%d", labelIndex))
}

type TypeName types.Name

func (TypeName) immediate() {}

func (TypeName) Kind() kinds.Kind { return kinds.Type }

func (t TypeName) String() string {
	return fmt.Sprintf("TypeName(%s)", string(t))
}

func (t TypeName) Value() *Value {
	return &Value{
		Type:    types.Kind(kinds.Type),
		Operand: TypeOperand(types.Name(t)),
	}
}

type Int int64

func (Int) immediate() {}

func (Int) Kind() kinds.Kind { return kinds.Int }

func (i Int) Value() *Value {
	return &Value{
		Type:    types.Kind(kinds.Int),
		Operand: IntOperand(i),
	}
}

func (i Int) String() string {
	return fmt.Sprintf("Int(%d)", int64(i))
}

type Float float64

func (Float) immediate() {}

func (Float) Kind() kinds.Kind { return kinds.Float }

func (f Float) Value() *Value {
	return &Value{
		Type:    types.Kind(kinds.Float),
		Operand: FloatOperand(f),
	}
}

type String string

func (String) Kind() kinds.Kind { return kinds.String }

func (s String) String() string {
	return fmt.Sprintf("String(%v)", string(s))
}

func (s String) Value() *Value {
	return &Value{
		Type:    types.Kind(kinds.String),
		Operand: StringOperand(s),
	}
}

type Bool bool

func (Bool) immediate() {}

func (Bool) Kind() kinds.Kind { return kinds.Bool }

func (b Bool) String() string {
	return fmt.Sprintf("Bool(%v)", bool(b))
}

func (b Bool) Value() *Value {
	return &Value{
		Type:    types.Kind(kinds.Bool),
		Operand: BoolOperand(b),
	}
}

type Addr uint64

func (a Addr) Offset(o Size) Addr {
	return Addr(int64(a) + int64(o))
}

func (a Addr) String() string {
	return fmt.Sprintf("0x%08x", uint64(a))
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
	Kind() kinds.Kind
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
	return fmt.Sprintf("(mov %v %v %v)", m.Size, m.Dst, m.Src)
}

type Convert[To, From any] struct {
	Register
}

type ConvertIntFloat = Convert[int64, float64]
type ConvertFloatInt = Convert[float64, int64]

type UnOp struct {
	Op   operators.Operator
	Kind kinds.Kind
	Dst  *Operand
	Src  *Operand
}

func (o UnOp) Name() string {
	return "uop"
}

func (o UnOp) String() string {
	return fmt.Sprintf("UnOp(%s) %v = %v %v", o.Op, o.Dst, o.Op, o.Src)
}

type BinOp struct {
	Op    operators.Operator
	Kind  kinds.Kind
	Dst   *Operand
	Left  *Operand
	Right *Operand
	Size  Size
}

func (o BinOp) Name() string {
	return "bop"
}

func (o BinOp) String() string {
	return fmt.Sprintf("(bop %v %v %v %v %v %v)", o.Size, o.Kind, o.Dst, o.Left, o.Op, o.Right)
}

type Ret struct {
	Args Size
}

func (r Ret) Name() string {
	return "ret"
}

func (r Ret) String() string {
	return fmt.Sprintf("(ret %d)", r.Args)
}

type Cal struct {
	Func *Operand `xc:"f"`
	Line int      `xc:"l"`
}

func (c Cal) String() string {
	return fmt.Sprintf("(cal %s %d)", c.Func, c.Line)
}

func (c Cal) Name() string {
	return "cal"
}

type Jmp struct {
	Cond   *Operand
	Target *Operand
}

func (j Jmp) Name() string {
	return "jmp"
}

func (j Jmp) String() string {
	return fmt.Sprintf("(jmp %v %v)", j.Target, j.Cond)
}

type Alc struct {
	Dst  *Operand
	Size *Operand
}

func (a Alc) Name() string {
	return "alc"
}

func (a Alc) String() string {
	return fmt.Sprintf("(alc %d %v)", a.Size, a.Dst)
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
	return fmt.Sprintf("(app %v %v %v %v)", a.Size, a.Dst, a.Src, a.Elem)
}
