package compiler

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/parser"
)

var (
	TypeInt    = NewDerivedType("int", nil, TypeKind(KindInt))
	TypeString = NewDerivedType("string", nil, TypeKind(KindString))
	TypeBool   = NewDerivedType("bool", nil, TypeKind(KindBool))
	TypeFloat  = NewDerivedType("float", nil, TypeKind(KindFloat))
	TypeAny    = NewDerivedType("any", nil, &InterfaceType{})
)

var (
	Nil = &Constant{
		name:               "nil",
		typ:                NilType,
		ConstantExpression: NewLiteral(NilValue),
	}
)

var (
	BuiltinLen    = &BuiltinSymbol{name: "len"}
	BuiltinAssert = &BuiltinSymbol{name: "assert"}
)

func BuiltinsSymbols() *SymbolScope {
	s := newScope(nil, "builtins")

	s.put(TypeInt)
	s.put(TypeBool)
	s.put(TypeString)
	s.put(TypeFloat)
	s.put(TypeAny)
	s.put(Nil)
	s.put(BuiltinLen)

	return s
}

func BuiltinValues(regs int, symbols *SymbolScope) *ValueScope {
	s := NewValueScope(regs, symbols)

	s.registerType(NilType)

	return s
}

const (
	VarInitFuncName = "__varinit"
)

type BuiltinSymbol struct {
	name string
	parser.Position
}

func (e *BuiltinSymbol) Name() string {
	return e.name
}

func (e *BuiltinSymbol) Type() Type {
	return &BuiltinType{name: e.name}
}

type BuiltinType struct {
	name string
}

func (*BuiltinType) Kind() Kind { return KindBuiltin }

func (t *BuiltinType) String() string {
	return fmt.Sprintf("<builtin %s>", t.name)
}

func (t *BuiltinType) GlobalName() TypeName {
	return TypeName(fmt.Sprintf("<builtin %s>", t.name))
}

func (t *BuiltinType) Size() Size {
	return 0
}

func (t *BuiltinType) Name() string {
	return t.name
}

type BuiltinExpression struct {
	Name string
	Args []Expression

	parser.Position
}

func (e *BuiltinExpression) Type() Type {
	switch e.Name {
	case "len":
		return TypeInt
	case "assert":
		return TypeVoid
	default:
		return UnknownType
	}
}
