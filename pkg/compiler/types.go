package compiler

import (
	"fmt"
	"strings"
)

type UnspecifiedKindType struct {
	kind Kind
}

func (t UnspecifiedKindType) Kind() Kind     { return t.kind }
func (t UnspecifiedKindType) Name() string   { return fmt.Sprintf("<unspecified %s>", t.kind) }
func (t UnspecifiedKindType) String() string { return fmt.Sprintf("<unspecified %s>", t.kind) }

var UnknownType = unknownType{}

type unknownType struct{}

func (unknownType) Name() string   { return "<unknown>" }
func (unknownType) String() string { return "<unknown>" }
func (unknownType) Kind() Kind     { return KindUnknown }

func resolveType(typ Type) Type {
	switch typ := typ.(type) {
	case *ReferencedType:
		return resolveType(typ.Dereference())
	default:
		return typ
	}
}

func typesEqual(t1, t2 Type) bool {
	if t1.Kind() != t2.Kind() {
		return false
	}

	switch t1 := t1.(type) {
	case *BasicType:
		switch t2 := t2.(type) {
		case *BasicType:
			return *t1 == *t2
		default:
			return false
		}
	case *DerivedType:
		switch t2 := t2.(type) {
		case *DerivedType:
			return t1.name == t2.name && t1.scope == t2.scope && TypesEqual(t1.Underlying(), t2.Underlying())
		default:
			return false
		}
	case *SliceType:
		switch t2 := t2.(type) {
		case *SliceType:
			return TypesEqual(t1.Elem(), t2.Elem())
		default:
			return false
		}
	case *MapType:
		switch t2 := t2.(type) {
		case *MapType:
			return TypesEqual(t1.Key(), t2.Key()) && TypesEqual(t1.Value(), t2.Value())
		default:
			return false
		}
	default:
		return false
	}
}

func TypesEqual(t1, t2 Type) bool {
	return typesEqual(resolveType(t1), resolveType(t2))
}

func IsTypeSpecified(s *Scope, typ Type) bool {
	switch typ := typ.(type) {
	case *BasicType:
		return IsKnownType(s, typ)
	case *DerivedType:
		return IsTypeSpecified(s, typ.Underlying())
	case *SliceType:
		return IsTypeSpecified(s, typ.Elem())
	case *MapType:
		return IsTypeSpecified(s, typ.Key()) && IsTypeSpecified(s, typ.Value())
	case *FunctionType:
		for _, param := range typ.Parameters() {
			if !IsTypeSpecified(s, param) {
				return false
			}
		}

		if typ.ret != nil {
			if !IsTypeSpecified(s, typ.ret) {
				return false
			}
		}

		return true
	case unknownType:
		return false
	default:
		return false
	}

}

func IsKnownType(s *Scope, typ Type) bool {
	t, ok := s.getType(typ.Name())
	if !ok {
		return false
	}

	return t == typ
}

type Kind int

const (
	KindUnknown Kind = iota
	KindBoolean
	KindInteger
	KindFloat
	KindString
	KindPointer
	KindMap
	KindStruct
	KindTuple
	KindSlice
	KindFunction

	// TODO: KindInterface
)

func (k Kind) String() string {
	switch k {
	case KindBoolean:
		return "bool"
	case KindInteger:
		return "int"
	case KindFloat:
		return "float"
	case KindString:
		return "string"
	case KindPointer:
		return "pointer"
	case KindMap:
		return "map"
	case KindStruct:
		return "struct"
	case KindTuple:
		return "tuple"
	case KindSlice:
		return "function"
	default:
		return "<unknown>"
	}
}

type Type interface {
	Kind() Kind
	Name() string

	String() string
}

type BasicType struct {
	name string
	kind Kind
}

func (t BasicType) Kind() Kind     { return t.kind }
func (t BasicType) Name() string   { return t.name }
func (t BasicType) String() string { return t.name }

type ReferencedType struct {
	s    *Scope
	name string
}

func (t ReferencedType) Name() string {
	return t.Dereference().Name()
}

func (t ReferencedType) Kind() Kind {
	return t.Dereference().Kind()
}

func (t ReferencedType) Dereference() Type {
	ref, ok := t.s.getType(t.name)
	if !ok {
		return UnknownType
	}

	return ref
}

func (t ReferencedType) String() string {
	return t.Dereference().String()
}

type DerivedType struct {
	name       string
	scope      string
	underlying Type
}

func (t DerivedType) Name() string {
	return t.name
}

func (t DerivedType) Kind() Kind {
	return t.underlying.Kind()
}

func (t DerivedType) String() string {
	return fmt.Sprintf("%s.%s", t.scope, t.name)
}

func (t DerivedType) Underlying() Type { return t.underlying }

func (t DerivedType) Scope() string {
	return t.scope
}

type PointerType struct {
	pointee Type
}

func (PointerType) Kind() Kind { return KindPointer }

func (t PointerType) Name() string {
	return fmt.Sprintf("*%s", t.Pointee().Name())
}

func (t PointerType) String() string {
	return fmt.Sprintf("*%s", t.Pointee().String())
}

func (t PointerType) Pointee() Type {
	return t.pointee
}

type SliceType struct {
	elem Type
}

func (t *SliceType) Kind() Kind { return KindSlice }

func (t *SliceType) Name() string { return fmt.Sprintf("[]%s", t.elem.Name()) }

func (t *SliceType) String() string { return fmt.Sprintf("[]%s", t.elem.String()) }

func (t *SliceType) Elem() Type { return t.elem }

type TupleType struct {
	elems []Type
}

func (t *TupleType) Kind() Kind { return KindTuple }

func (t *TupleType) Name() string {
	var names []string
	for _, elem := range t.elems {
		names = append(names, elem.Name())
	}

	return fmt.Sprintf("(%s)", strings.Join(names, ", "))
}

func (t *TupleType) String() string {
	var strs []string
	for _, elem := range t.elems {
		strs = append(strs, elem.String())
	}

	return fmt.Sprintf("(%s)", strings.Join(strs, ", "))
}

func (t *TupleType) Elems() []Type { return t.elems }

type MapType struct {
	key   Type
	value Type
}

func (t *MapType) Name() string {
	return fmt.Sprintf("map[%s]%s", t.key.Name(), t.value.Name())
}

func (*MapType) Kind() Kind { return KindMap }

func (t *MapType) String() string {
	return fmt.Sprintf("map[%s]%s", t.key.String(), t.value.String())
}

func (t *MapType) Key() Type {
	return t.key
}

func (t *MapType) Value() Type {
	return t.value
}

type StructType struct {
	BasicType

	fields []StructField
}

func (t StructType) Fields() []StructField {
	return t.fields
}

type StructField struct {
	Name string
	Type Type
}

type FunctionType struct {
	name       string
	scope      string
	parameters []Type
	ret        Type
}

func (t FunctionType) Kind() Kind { return KindFunction }

func (t FunctionType) Name() string { return fmt.Sprintf("%s.%s", t.scope, t.name) }

func (t FunctionType) String() string {
	params := make([]string, len(t.parameters))
	for _, param := range t.parameters {
		params = append(params, param.String())
	}

	var retStr string
	if t.ret != nil {
		retStr = " " + t.ret.String()
	}

	return fmt.Sprintf("func(%s)", strings.Join(params, ", "), retStr)
}

func (t FunctionType) Parameters() []Type {
	return t.parameters
}

func (t FunctionType) Return() Type {
	return t.ret
}
