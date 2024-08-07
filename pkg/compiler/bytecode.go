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

type OperandKind int

const (
	OperandKindImmediate OperandKind = 0
	OperandKindRegister  OperandKind = 1
	OperandKindIndirect  OperandKind = 2
	OperandKindExtern    OperandKind = 3
)

func (s OperandKind) MarshalXenon() ([]byte, error) {
	switch s {
	case OperandKindImmediate:
		return []byte("I"), nil
	case OperandKindRegister:
		return []byte("R"), nil
	case OperandKindIndirect:
		return []byte("*"), nil
	default:
		return nil, fmt.Errorf("invalid value source %x", s)
	}
}

type Indirect struct {
	Base   *Operand   `xc:"b"`
	Offset AddrOffset `xc:"o"`
}

func (i Indirect) String() string {
	return fmt.Sprintf("[%s %s]", i.Base, i.Offset)
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

type Addr uint64

func (Addr) immediate() {}

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

func (Nop) Name() string {
	return "nop"
}

type Mov struct {
	Src  *Operand `xc:"s"`
	Dst  *Operand `xc:"d"`
	Size int      `xc:"c"`
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

type Operand struct {
	Kind  OperandKind `xc:"k"`
	Value any         `xc:"v"`
}

func (o *Operand) Offset(offset AddrOffset) *Operand {
	if offset == 0 {
		return o
	}

	return &Operand{
		Kind: OperandKindIndirect,
		Value: Indirect{
			Base:   o.Value.(Indirect).Base,
			Offset: o.Value.(Indirect).Offset + offset,
		},
	}
}

func (o *Operand) Dereference() *Operand {
	return &Operand{
		Kind: OperandKindIndirect,
		Value: Indirect{
			Base:   o,
			Offset: 0,
		},
	}
}

func (o Operand) String() string {
	return fmt.Sprintf("%s", o.Value)
}

func ImmediateOperand(imm Immediate) *Operand {
	return &Operand{
		Value: imm,
		Kind:  OperandKindImmediate,
	}
}

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

type Return struct{}

func (r Return) Name() string {
	return "ret"
}

func (r Return) String() string {
	return fmt.Sprintf("RET")
}

type CallExtern struct {
	Func string `xc:"f"`
}

func (e CallExtern) String() string {
	return fmt.Sprintf("CALL %s", e.Func)
}

func (e CallExtern) Name() string {
	return "ext"
}

type Call struct {
	Func *Operand `xc:"f"`
}

func (c Call) String() string {
	return fmt.Sprintf("CALL %s", c.Func)
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

type LAddr struct {
	Dst *Operand `xc:"d"`
	Src *Operand `xc:"s"`
}

func (LAddr) Name() string {
	return "laddr"
}

func (a LAddr) String() string {
	return fmt.Sprintf("LADDR %s", a.Src)
}
