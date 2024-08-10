package compiler

import (
	"cmp"
	"fmt"
	"slices"
	"strings"

	"golang.org/x/exp/maps"
)

var UnknownType = unknownType{}

type unknownType struct{}

func (unknownType) String() string { return "<unknown>" }
func (unknownType) Kind() Kind     { return KindUnknown }
func (unknownType) Size() Size     { return 0 }

var VoidType = voidType{}

type voidType struct{}

func (voidType) String() string { return "<void>" }
func (voidType) Kind() Kind     { return KindVoid }
func (voidType) Size() Size     { return 0 }

func IsAssignableTo(v, to Type) bool {
	if v.Kind() == KindTypeConversion {
		return false
	}

	if to == nil {
		return true
	}

	if TypesEqual(v, to) {
		return true
	}

	if v.Kind() == to.Kind() {
		if _, ok := v.(TypeKind); ok {
			return true
		}

		if _, ok := to.(TypeKind); ok {
			return true
		}
	}

	if to, ok := to.(*InterfaceType); ok {
		return to.ImplementedBy(v)
	}

	return false
}

func IsConvertibleTo(v, to Type) bool {
	if IsAssignableTo(v, to) {
		return true
	}

	if v.Kind() == KindTypeConversion {
		return false
	}

	if v.Kind().IsNumeric() && to.Kind().IsNumeric() {
		return true
	}

	return TypesEqual(BaseType(v), BaseType(to))
}

func BaseType(typ Type) Type {
	switch typ := typ.(type) {
	case *ReferencedType:
		return resolveType(typ.Dereference())
	case *DerivedType:
		return resolveType(typ.Underlying())
	default:
		return typ
	}
}

func resolveType(typ Type) Type {
	switch typ := typ.(type) {
	case *ReferencedType:
		res := resolveType(typ.Dereference())
		if res == UnknownType {
			return typ
		}

		return res
	default:
		return typ
	}
}

func typesEqual(t1, t2 Type) bool {
	if t1.Kind() != t2.Kind() {
		return false
	}

	switch t1 := t1.(type) {
	case TypeKind:
		return t1 == t2
	case *BasicType:
		switch t2 := t2.(type) {
		case *BasicType:
			return *t1 == *t2
		default:
			return false
		}
	case *DerivedType:
		return t1 == t2
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

func IsUnspecified(typ Type) bool {
	if typ == nil {
		return true
	}

	_, ok := resolveType(typ).(TypeKind)
	return ok
}

func IsTypeResolvable(s *SymbolScope, typ Type) bool {
	switch typ := resolveType(typ).(type) {
	case TypeKind:
		return false
	case *BasicType:
		_, ok := s.getType(typ.String())
		return ok
	case *PointerType:
		return IsTypeResolvable(s, typ.Pointee())
	case *DerivedType:
		return IsTypeResolvable(s, typ.Underlying())
	case *SliceType:
		return IsTypeResolvable(s, typ.Elem())
	case *MapType:
		return IsTypeResolvable(s, typ.Key()) && IsTypeResolvable(s, typ.Value())
	case *TupleType:
		for _, subType := range typ.Elems() {
			if !IsTypeResolvable(s, subType) {
				return false
			}
		}

		return true
	case *FunctionType:
		for _, param := range typ.Parameters {
			if !IsTypeResolvable(s, param) {
				return false
			}
		}

		if typ.Return != nil {
			if !IsTypeResolvable(s, typ.Return) {
				return false
			}
		}

		return true
	case *InterfaceType:
		for _, method := range typ.Methods() {
			if !IsTypeResolvable(s, method) {
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

type TypeKind Kind

func (t TypeKind) String() string {
	return t.Name()
}

func (t TypeKind) Name() string {
	return fmt.Sprintf("<kind %s>", t.Kind())
}

func (t TypeKind) Kind() Kind {
	return Kind(t)
}

func (t TypeKind) Size() Size {
	switch t.Kind() {
	case KindVoid:
		return 0
	case KindBool, KindInt, KindFloat, KindString, KindPointer:
		return 1
	case KindFunction:
		return 4
	case KindSlice:
		return 3
	default:
		return -1
	}
}

type Kind int

const (
	KindUnknown Kind = iota
	KindVoid
	KindBool
	KindInt
	KindFloat
	KindString
	KindPointer
	KindMap
	KindStruct
	KindTuple
	KindSlice
	KindFunction
	KindInterface
	KindTypeConversion
)

func (k Kind) String() string {
	switch k {
	case KindBool:
		return "bool"
	case KindInt:
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
	case KindInterface:
		return "interface"
	case KindTypeConversion:
		return "type conversion"
	default:
		return "<unknown>"
	}
}

func (k Kind) IsNumeric() bool {
	return k == KindInt || k == KindFloat
}

type Type interface {
	Kind() Kind
	String() string
	Size() Size
}

type BasicType struct {
	name string
	kind Kind
	size Size
}

func (t BasicType) Kind() Kind     { return t.kind }
func (t BasicType) Name() string   { return t.name }
func (t BasicType) String() string { return t.name }
func (t BasicType) Size() Size     { return t.size }

type ReferencedType struct {
	s    *SymbolScope
	name string
	pkg  string
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
	return t.name
}

func (t ReferencedType) Size() Size {
	return t.Dereference().Size()
}

type MethodSetEntry struct {
	Name       string
	Parameters []Type
	Return     Type
}

func (e MethodSetEntry) Equal(o MethodSetEntry) bool {
	if e.Name != o.Name {
		return false
	}

	if !slices.EqualFunc(e.Parameters, o.Parameters, TypesEqual) {
		return false
	}

	if !TypesEqual(e.Return, o.Return) {
		return false
	}

	return true
}

type MethodSet []MethodSetEntry

func (m *MethodSet) Add(name string, params []Type, ret Type) error {
	entry := MethodSetEntry{
		Name:       name,
		Parameters: params,
		Return:     ret,
	}

	if slices.ContainsFunc(*m, entry.Equal) {
		return fmt.Errorf("method set already contains method %q", name)
	}

	*m = append(*m, entry)

	slices.SortStableFunc(*m, func(a, b MethodSetEntry) int {
		return cmp.Compare(a.Name, b.Name)
	})

	return nil
}

func (m MethodSet) Equal(o MethodSet) bool {
	return slices.EqualFunc(m, o, MethodSetEntry.Equal)
}

func (m MethodSet) Subset(o MethodSet) bool {
	for _, entry := range m {
		if !slices.ContainsFunc(o, entry.Equal) {
			return false
		}
	}

	return true
}

type DerivedType struct {
	name       string
	methods    MethodSet
	underlying Type
}

func (t *DerivedType) Name() string {
	return t.name
}

func (t *DerivedType) Kind() Kind {
	return t.underlying.Kind()
}

func (t *DerivedType) String() string {
	return t.name
}

func (t *DerivedType) Underlying() Type { return t.underlying }

func (t *DerivedType) Size() Size {
	return t.underlying.Size()
}

func (t *DerivedType) Methods(ptr bool) MethodSet {
	m := make(MethodSet)
	for k, v := range t.methods {
		if v.Receiver.Kind() == KindPointer && !ptr {
			continue
		}

		m[k] = v
	}
	return m
}

type PointerType struct {
	pointee Type
}

func NewPointerType(pointee Type) *PointerType {
	return &PointerType{pointee: pointee}
}

func (PointerType) Kind() Kind { return KindPointer }
func (PointerType) Size() Size { return 1 }

func (t PointerType) String() string {
	return fmt.Sprintf("*%s", t.Pointee().String())
}

func (t PointerType) Pointee() Type {
	return t.pointee
}

type SliceType struct {
	elem Type
}

func (t SliceType) Kind() Kind { return KindSlice }

func (t SliceType) String() string { return fmt.Sprintf("[]%s", t.elem.String()) }

func (t SliceType) Elem() Type { return t.elem }
func (SliceType) Size() Size   { return 3 }

var sliceHeader = NewTupleType(
	IntType,
	IntType,
	NewPointerType(VoidType),
)

type TupleType struct {
	elems []Type
}

func NewTupleType(elems ...Type) *TupleType {
	return &TupleType{elems: elems}
}

func (t TupleType) Kind() Kind { return KindTuple }

func (t TupleType) String() string {
	var strs []string
	for _, elem := range t.elems {
		strs = append(strs, elem.String())
	}

	return fmt.Sprintf("(%s)", strings.Join(strs, ", "))
}

func (t TupleType) Elems() []Type { return t.elems }
func (t TupleType) ElemOffset(index int) Size {
	var size Size
	for _, typ := range t.elems[:index] {
		size += typ.Size()
	}

	return size
}
func (t TupleType) Size() Size {
	return t.ElemOffset(len(t.elems))
}

type MapType struct {
	key   Type
	value Type
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

func (*MapType) Size() Size {
	return 1
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
	Receiver   Type
	Parameters []Type
	Return     Type
}

func (t *FunctionType) MethodEqual(o *FunctionType) bool {
	if len(t.Parameters) != len(o.Parameters) {
		return false
	}

	for i := range len(t.Parameters) {
		if !TypesEqual(t.Parameters[i], o.Parameters[i]) {
			return false
		}
	}

	return TypesEqual(t.Return, o.Return)
}

func (t *FunctionType) Kind() Kind { return KindFunction }

func (t *FunctionType) String() string {
	params := make([]string, 0, len(t.Parameters))
	for _, param := range t.Parameters {
		params = append(params, param.String())
	}

	var retStr string
	if t.Return != nil {
		retStr = t.Return.String()
	}

	return fmt.Sprintf("func(%s) %s", strings.Join(params, ", "), retStr)
}

func (FunctionType) Size() Size {
	return 4
}

type InterfaceType struct {
	methods MethodSet
}

func (t *InterfaceType) String() string {
	methods := t.Methods()
	names := maps.Keys(methods)
	slices.Sort(names)

	var parts []string
	for _, name := range names {
		parts = append(parts, fmt.Sprintf("%s %s", name, methods[name]))
	}

	return fmt.Sprintf("interface{%s}", strings.Join(parts, "; "))
}

func (t *InterfaceType) Methods() MethodSet {
	return t.methods
}

func (t *InterfaceType) Kind() Kind {
	return KindInterface
}

func (*InterfaceType) Size() Size {
	return 2
}

var interfaceTuple = NewTupleType(
	IntType,
	NewPointerType(VoidType),
)

func (t *InterfaceType) ImplementedBy(i Type) bool {
	switch i := resolveType(i).(type) {
	case *DerivedType:
		return t.Methods().Subset(i.Methods(false))
	case *PointerType:
		switch pointee := i.Pointee().(type) {
		case *DerivedType:
			return t.Methods().Subset(pointee.Methods(true))
		default:
			return len(t.Methods()) == 0
		}
	default:
		return len(t.Methods()) == 0
	}
}

type TypeConversionType struct {
	Type Type
}

func (*TypeConversionType) Kind() Kind       { return KindTypeConversion }
func (*TypeConversionType) Size() Size       { return 0 }
func (t *TypeConversionType) String() string { return fmt.Sprintf("%s()", t.Type) }
