package air

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler/operators"
	"github.com/rhino1998/aeon/pkg/compiler/types"
	"golang.org/x/exp/constraints"
)

type OperandKind int

const (
	OperandKindUnknown OperandKind = iota
	OperandKindImmediate
	OperandKindRegister
	OperandKindIndirect
	OperandKindBinary
	OperandKindUnary
	OperandKindVTableLookup

	// stripped out before end of compilation
	OperandKindLabel
	OperandKindType
	OperandKindString
	OperandKindSymbol
	OperandKindLocal
	OperandKindTemp
	OperandKindGlobal
	OperandKindConstant
)

func (s OperandKind) String() string {
	switch s {
	case OperandKindImmediate:
		return "I"
	case OperandKindRegister:
		return "R"
	case OperandKindIndirect:
		return "@"
	case OperandKindBinary:
		return "B"
	case OperandKindUnary:
		return "U"
	case OperandKindVTableLookup:
		return "V"
	case OperandKindLabel:
		return "<compiler label>"
	case OperandKindType:
		return "<compiler type>"
	case OperandKindString:
		return "<compiler string>"
	case OperandKindLocal:
		return "<compiler local>"
	case OperandKindTemp:
		return "<compiler temp>"
	case OperandKindGlobal:
		return "<compiler global>"
	default:
		return "?"
	}
}

const (
	RegisterPC Register = 0
	RegisterSP Register = 1
	RegisterFP Register = 2
)

type Register int64

func (r Register) String() string {
	return fmt.Sprintf("reg%02x", int64(r))
}

func RegisterOperand(reg Register) *Operand {
	return &Operand{
		Kind:  OperandKindRegister,
		Value: reg,
	}
}

var (
	OperandRegisterPC = RegisterOperand(RegisterPC)
	OperandRegisterSP = RegisterOperand(RegisterSP)
	OperandRegisterFP = RegisterOperand(RegisterFP)
)

func TypeOperand(name types.Name) *Operand {
	return &Operand{
		Kind:  OperandKindType,
		Value: name,
	}
}
func FloatOperand(f Float) *Operand {
	return &Operand{
		Kind:  OperandKindImmediate,
		Value: f,
	}
}

func StringOperand(str String) *Operand {
	return &Operand{
		Kind:  OperandKindString,
		Value: str,
	}
}

func LabelOperand(label Label) *Operand {
	return &Operand{
		Kind:  OperandKindLabel,
		Value: label,
	}
}

func LocalOperand(v Variable) *Operand {
	return &Operand{
		Kind:  OperandKindLocal,
		Value: v,
	}
}

func TempOperand(v Variable) *Operand {
	return &Operand{
		Kind:  OperandKindTemp,
		Value: v,
	}
}

func GlobalOperand(v Variable) *Operand {
	return &Operand{
		Kind:  OperandKindGlobal,
		Value: v,
	}
}

func ConstantOperand(v Variable) *Operand {
	return &Operand{
		Kind:  OperandKindConstant,
		Value: v,
	}
}

type VTableLookup struct {
	Type   *Operand `xc:"t"`
	Method *Operand `xc:"m"`
}

type Indirect struct {
	Ptr *Operand `xc:"p"`
}

func (i Indirect) String() string {
	return fmt.Sprintf("[%s]", i.Ptr)
}

type UnaryOperand struct {
	Op operators.Operator `xc:"o"`
	A  *Operand           `xc:"a"`
}

func (n UnaryOperand) String() string { return fmt.Sprintf("%s%s", n.Op, n.A) }

type BinaryOperand struct {
	Left  *Operand           `xc:"a"`
	Op    operators.Operator `xc:"o"`
	Right *Operand           `xc:"b"`
}

func (o BinaryOperand) String() string {
	return fmt.Sprintf("(%s %s %s)", o.Left, o.Op, o.Right)
}

type Operand struct {
	Kind  OperandKind `xc:"k"`
	Value any         `xc:"v"`
}

type operandWalkFunc func(*Operand) (*Operand, error)

func (o *Operand) walk(w operandWalkFunc) (*Operand, error) {
	var err error
	switch o.Kind {
	case OperandKindIndirect:
		v := o.Value.(Indirect)
		v.Ptr, err = v.Ptr.walk(w)
		if err != nil {
			return nil, err
		}

		o.Value = v

		return w(o)
	case OperandKindVTableLookup:
		v := o.Value.(VTableLookup)
		v.Type, err = v.Type.walk(w)
		if err != nil {
			return nil, err
		}

		v.Method, err = w(v.Method)
		if err != nil {
			return nil, err
		}

		o.Value = v

		return w(o)
	case OperandKindBinary:
		v := o.Value.(BinaryOperand)
		v.Left, err = v.Left.walk(w)
		if err != nil {
			return nil, err
		}

		v.Right, err = v.Right.walk(w)
		if err != nil {
			return nil, err
		}

		o.Value = v

		return w(o)
	case OperandKindUnary:
		v := o.Value.(UnaryOperand)
		v.A, err = v.A.walk(w)
		if err != nil {
			return nil, err
		}

		o.Value = v

		return w(o)
	case OperandKindImmediate, OperandKindLabel, OperandKindString, OperandKindRegister, OperandKindType, OperandKindLocal, OperandKindGlobal, OperandKindTemp:
		return w(o)
	default:
		return nil, fmt.Errorf("unhandled operand type %q", o.Kind)
	}
}

func NewVTableLookup(typ, method *Operand) *Operand {
	return &Operand{
		Kind: OperandKindVTableLookup,
		Value: VTableLookup{
			Type:   typ,
			Method: method,
		},
	}
}

func (o *Operand) VTableLookup(method *Operand) *Operand {
	return &Operand{
		Kind: OperandKindVTableLookup,
		Value: VTableLookup{
			Type:   o,
			Method: method,
		},
	}
}

func (o *Operand) OffsetReference(offset Size) *Operand {
	if offset == 0 {
		return o
	}

	return o.AddressOf().AddConst(offset).Dereference()
}

func (o *Operand) AddConst(offset Size) *Operand {
	return o.Add(IntOperand(offset))
}

func (o *Operand) Add(r *Operand) *Operand {
	return &Operand{
		Kind: OperandKindBinary,
		Value: BinaryOperand{
			Left:  o,
			Op:    operators.Addition,
			Right: r,
		},
	}
}

func (o *Operand) Mul(size *Operand) *Operand {
	return &Operand{
		Kind: OperandKindBinary,
		Value: BinaryOperand{
			Left:  o,
			Op:    "*",
			Right: size,
		},
	}
}

func (o *Operand) AddressOf() *Operand {
	return o.Value.(Indirect).Ptr
}

func (o *Operand) Dereference() *Operand {
	return &Operand{
		Kind: OperandKindIndirect,
		Value: Indirect{
			Ptr: o,
		},
	}
}

func (o *Operand) Not() *Operand {
	return &Operand{
		Kind: OperandKindUnary,
		Value: UnaryOperand{
			Op: operators.Not,
			A:  o,
		},
	}
}

func (o *Operand) String() string {
	return fmt.Sprintf("%s", o.Value)
}

func (o *Operand) Bound(bound *Operand) *Operand {
	return &Operand{
		Kind: OperandKindBinary,
		Value: BinaryOperand{
			Left:  o,
			Op:    operators.BoundsCheck,
			Right: bound,
		},
	}
}

func (o *Operand) Equal(r *Operand) *Operand {
	return &Operand{
		Kind: OperandKindBinary,
		Value: BinaryOperand{
			Left:  o,
			Op:    operators.Equal,
			Right: r,
		},
	}
}

func (o *Operand) NotEqual(r *Operand) *Operand {
	return &Operand{
		Kind: OperandKindBinary,
		Value: BinaryOperand{
			Left:  o,
			Op:    operators.NotEqual,
			Right: r,
		},
	}
}

func IntOperand[I constraints.Integer](i I) *Operand {
	return &Operand{
		Value: Int(i),
		Kind:  OperandKindImmediate,
	}
}

func BoolOperand[B ~bool](b B) *Operand {
	return &Operand{
		Value: Bool(b),
		Kind:  OperandKindImmediate,
	}
}

func ImmediateOperand(imm Immediate) *Operand {
	return &Operand{
		Value: imm,
		Kind:  OperandKindImmediate,
	}
}

func (o *Operand) Optimize() *Operand {
	switch v := o.Value.(type) {
	case Indirect:
		v.Ptr = v.Ptr.Optimize()
		o.Value = v
		return o
	case BinaryOperand:
		v.Left = v.Left.Optimize()
		v.Right = v.Right.Optimize()

		o.Value = v

		if v.Left.Kind == OperandKindImmediate && v.Right.Kind == OperandKindImmediate && v.Op == "+" {
			return ImmediateOperand(v.Left.Value.(Int) + v.Right.Value.(Int))
		}

		return o
	case UnaryOperand:
		v.A = v.A.Optimize()

		o.Value = v

		if v.A.Kind == OperandKindImmediate {
			return ImmediateOperand(!v.A.Value.(Bool))
		}

		return o
	default:
		return o
	}
}
