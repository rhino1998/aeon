package compiler

import "fmt"

type OperandKind int

const (
	OperandKindUnknown OperandKind = iota
	OperandKindImmediate
	OperandKindRegister
	OperandKindIndirect
	OperandKindBinary
	OperandKindNot
	OperandKindVTableLookup

	// stripped out before end of compilation
	OperandKindLabel
	OperandKindType
	OperandKindString
	OperandKindSymbol
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
	case OperandKindNot:
		return "!"
	case OperandKindVTableLookup:
		return "V"
	case OperandKindLabel:
		return "<compiler label>"
	case OperandKindType:
		return "<compiler type>"
	case OperandKindString:
		return "<compiler string>"
	default:
		return "?"
	}
}

func TypeOperand(name TypeName) *Operand {
	return &Operand{
		Kind:  OperandKindType,
		Value: name,
	}
}

func StringOperand(name String) *Operand {
	return &Operand{
		Kind:  OperandKindString,
		Value: name,
	}
}

func LabelOperand(label Label) *Operand {
	return &Operand{
		Kind:  OperandKindLabel,
		Value: label,
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

type Not struct {
	A *Operand `xc:"a"`
}

func (n Not) String() string { return fmt.Sprintf("!%s", n.A) }

type BinaryOperand struct {
	A  *Operand `xc:"a"`
	Op Operator `xc:"o"`
	B  *Operand `xc:"b"`
}

func (o BinaryOperand) String() string {
	return fmt.Sprintf("(%s %s %s)", o.A, o.Op, o.B)
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
		v.A, err = v.A.walk(w)
		if err != nil {
			return nil, err
		}

		v.B, err = v.B.walk(w)
		if err != nil {
			return nil, err
		}

		o.Value = v

		return w(o)
	case OperandKindNot:
		v := o.Value.(Not)
		v.A, err = v.A.walk(w)
		if err != nil {
			return nil, err
		}

		o.Value = v

		return w(o)
	case OperandKindImmediate, OperandKindLabel, OperandKindString, OperandKindRegister, OperandKindType:
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

	return o.AddressOf().ConstOffset(offset).Dereference()
}

func (o *Operand) ConstOffset(offset Size) *Operand {
	if offset == 0 {
		return o
	}

	return o.Offset(ImmediateOperand(Int(offset)))
}

func (o *Operand) Offset(offset *Operand) *Operand {
	return &Operand{
		Kind: OperandKindBinary,
		Value: BinaryOperand{
			A:  o,
			Op: "+",
			B:  offset,
		},
	}
}

func (o *Operand) Stride(offset *Operand) *Operand {
	return &Operand{
		Kind: OperandKindBinary,
		Value: BinaryOperand{
			A:  o,
			Op: "*",
			B:  offset,
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
		Kind: OperandKindNot,
		Value: Not{
			A: o,
		},
	}
}

func (o *Operand) String() string {
	return fmt.Sprintf("%s", o.Value)
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
		v.A = v.A.Optimize()
		v.B = v.B.Optimize()

		o.Value = v

		if v.A.Kind == OperandKindImmediate && v.B.Kind == OperandKindImmediate && v.Op == "+" {
			return ImmediateOperand(v.A.Value.(Int) + v.B.Value.(Int))
		} else if v.A.Kind == OperandKindBinary && v.Op == "+" {
			rot := &Operand{
				Kind: OperandKindBinary,
				Value: BinaryOperand{
					A:  v.A.Value.(BinaryOperand).A,
					Op: "+",
					B:  v.A.Value.(BinaryOperand).B.Offset(v.B),
				},
			}

			rot = rot.Optimize()

			return rot
		}

		return o
	case Not:
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
