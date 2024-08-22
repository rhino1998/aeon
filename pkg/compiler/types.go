package compiler

import (
	"cmp"
	"fmt"
	"slices"
	"strings"
	"unicode"

	"github.com/rhino1998/aeon/pkg/parser"
)

type TypeName string

func (TypeName) Kind() Kind {
	return KindType
}

func (n TypeName) String() string {
	return fmt.Sprintf("Type(%s)", string(n))
}

func (n TypeName) Location(vs *ValueScope) *Location {
	typ, ok := vs.types[n]
	if !ok {
		panic(fmt.Errorf("bug: failed to resolve type %s", n))
	}
	return vs.typeName(typ)
}

var UnknownType = unknownType{}

type unknownType struct{}

func (unknownType) String() string       { return "<unknown>" }
func (unknownType) GlobalName() TypeName { return "<unknown>" }
func (unknownType) Kind() Kind           { return KindUnknown }
func (unknownType) Size() Size           { return 0 }

var TypeVoid = voidType{}

type voidType struct{}

func (voidType) String() string       { return "<void>" }
func (voidType) GlobalName() TypeName { return "<void>" }
func (voidType) Kind() Kind           { return KindVoid }
func (voidType) Size() Size           { return 0 }

var NilType = nilType{}

type nilType struct{}

func (nilType) String() string       { return "<nil>" }
func (nilType) GlobalName() TypeName { return "<nil>" }
func (nilType) Kind() Kind           { return KindNil }
func (nilType) Size() Size           { return 0 }

var NilValue = nilValue{}

type nilValue struct{}

func (nilValue) immediate() {}

func (nilValue) Kind() Kind { return KindNil }
func (v nilValue) Location(vs *ValueScope) *Location {
	panic("bug: evaluated nil location")
}

func IsValidMethodReceiverType(t Type) bool {
	switch t.Kind() {
	case KindInterface, KindVoid, KindNil, KindUnknown:
		return false
	default:
	}

	switch t := dereferenceType(t).(type) {
	case *DerivedType:
		return true
	case *PointerType:
		switch dereferenceType(t.Pointee()).(type) {
		case *DerivedType:
			return true
		}

	}

	return false
}

func IsAssignableTo(v, to Type) bool {
	if v.Kind() == KindType || v.Kind() == KindBuiltin {
		return false
	}

	if to == nil {
		return true
	}

	if TypesEqual(v, to) {
		return true
	}

	if v.Kind() == to.Kind() {
		if _, ok := to.(TypeKind); ok {
			return true
		}

		if _, ok := v.(TypeKind); ok {
			return true
		}
	}

	if v.Kind() == KindNil {
		return true
	}

	if to, ok := BaseType(to).(*InterfaceType); ok {
		return to.ImplementedBy(v)
	}

	return false
}

func IsConvertibleTo(v, to Type) bool {
	if IsAssignableTo(v, to) {
		return true
	}

	if v.Kind() == KindType {
		return false
	}

	if v.Kind().IsNumeric() && to.Kind().IsNumeric() {
		return true
	}

	return TypesEqual(BaseType(v), BaseType(to))
}

func BaseType(typ Type) Type {
	switch typ := dereferenceType(typ).(type) {
	case *ReferencedType:
		return BaseType(typ.Dereference())
	case *DerivedType:
		return BaseType(typ.underlying)
	case *PointerType:
		return NewPointerType(BaseType(typ.pointee))
	case *ArrayType:
		return &ArrayType{
			length: typ.length,

			elem: BaseType(typ.elem),
		}
	default:
		return typ
	}
}

func dereferenceType(typ Type) Type {
	switch typ := typ.(type) {
	case *ReferencedType:
		res := dereferenceType(typ.Dereference())
		if res == UnknownType {
			return typ
		}

		return res
	case *ParenthesizedType:
		return dereferenceType(typ.Type)
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
	case *ParenthesizedType:
		return resolveType(typ.Type)
	case *DerivedType:
		return resolveType(typ.underlying)
	default:
		return typ
	}
}

func typesEqual(t1, t2 Type) bool {
	if t1.Kind() != t2.Kind() {
		return false
	}

	switch t1 := t1.(type) {
	case nilType:
		return t1 == t2
	case voidType:
		return t1 == t2
	case TypeKind:
		return t1 == t2
	case *PointerType:
		switch t2 := t2.(type) {
		case *PointerType:
			return TypesEqual(t1.Pointee(), t2.Pointee())
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
	case *ArrayType:
		switch t2 := t2.(type) {
		case *ArrayType:
			return t1.Length() == t2.Length() && TypesEqual(t1.Elem(), t2.Elem())
		default:
			return false
		}
	case *TupleType:
		switch t2 := t2.(type) {
		case *TupleType:
			return slices.EqualFunc(t1.Elems(), t2.Elems(), TypesEqual)
		default:
			return false
		}
	case *FunctionType:
		switch t2 := t2.(type) {
		case *FunctionType:
			if !TypesEqual(t1.Receiver, t2.Receiver) {
				return false
			}

			if !slices.EqualFunc(t1.Parameters, t2.Parameters, TypesEqual) {
				return false
			}

			if !TypesEqual(t1.Receiver, t2.Receiver) {
				return false
			}

			if !TypesEqual(t1.Return, t2.Return) {
				return false
			}

			return true
		default:
			return false
		}
	default:
		return false
	}
}

func TypesEqual(t1, t2 Type) bool {
	return typesEqual(dereferenceType(t1), dereferenceType(t2))
}

func IsUnspecified(typ Type) bool {
	if typ == nil {
		return true
	}

	_, ok := dereferenceType(typ).(TypeKind)
	return ok
}

func IsTypeResolvable(typ Type) bool {
	switch typ := dereferenceType(typ).(type) {
	case TypeKind:
		return true
	case voidType:
		return true
	case nilType:
		return true
	case *PointerType:
		return IsTypeResolvable(typ.Pointee())
	case *DerivedType:
		return IsTypeResolvable(typ.Underlying())
	case *SliceType:
		return IsTypeResolvable(typ.Elem())
	case *ArrayType:
		return IsTypeResolvable(typ.Elem())
	case *MapType:
		return IsTypeResolvable(typ.Key()) && IsTypeResolvable(typ.Value())
	case *TupleType:
		for _, subType := range typ.Elems() {
			if !IsTypeResolvable(subType) {
				return false
			}
		}

		return true
	case *FunctionType:
		for _, param := range typ.Parameters {
			if !IsTypeResolvable(param) {
				return false
			}
		}

		if !IsTypeResolvable(typ.Receiver) {
			return false
		}

		if !IsTypeResolvable(typ.Return) {
			return false
		}

		return true
	case *InterfaceType:
		for _, method := range typ.Methods() {
			for _, param := range method.Parameters {
				if !IsTypeResolvable(param) {
					return false
				}
			}

			if !IsTypeResolvable(method.Return) {
				return false
			}
		}

		return true
	case *StructType:
		for _, field := range typ.Fields() {
			if !IsTypeResolvable(field.Type) {
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
	return string(t.GlobalName())
}

func (t TypeKind) GlobalName() TypeName {
	return TypeName(fmt.Sprintf("<kind %s>", t.Kind()))
}

func (t TypeKind) Kind() Kind {
	return Kind(t)
}

func (t TypeKind) Size() Size {
	switch t.Kind() {
	case KindVoid, KindNil:
		return 0
	case KindBool, KindInt, KindFloat, KindString, KindPointer, KindType:
		return 1
	case KindFunction:
		return 1
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
	KindNil
	KindBool
	KindInt
	KindFloat
	KindString
	KindPointer
	KindMap
	KindStruct
	KindTuple
	KindSlice
	KindArray
	KindFunction
	KindInterface
	KindType
	KindBuiltin
	KindVariadic
)

func (k Kind) IsOperand() bool {
	return k == KindBool || k == KindInt || k == KindFloat || k == KindString || k == KindPointer
}

func (k Kind) IsPrimitive() bool {
	// TODO: maybe pointers here too
	return k == KindBool || k == KindInt || k == KindFloat
}

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
		return "slice"
	case KindArray:
		return "array"
	case KindFunction:
		return "function"
	case KindInterface:
		return "interface"
	case KindType:
		return "type"
	case KindNil:
		return "<nil>"
	case KindVariadic:
		return "<...>"
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
	GlobalName() TypeName
	Size() Size
}

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

func (t ReferencedType) GlobalName() TypeName {
	return t.Dereference().GlobalName()
}

func (t ReferencedType) Size() Size {
	return t.Dereference().Size()
}

type Method struct {
	Name       string
	Receiver   Type
	Parameters []Type
	Return     Type
}

func (e Method) BoundType() *FunctionType {
	return &FunctionType{
		Receiver:   TypeVoid,
		Parameters: e.Parameters,
		Return:     e.Return,
	}
}

func (e Method) String() string {
	params := make([]string, 0, len(e.Parameters))
	for _, param := range e.Parameters {
		params = append(params, param.String())
	}

	var retStr string
	if e.Return != TypeVoid {
		retStr = fmt.Sprintf(" %s", e.Return)

	}

	return fmt.Sprintf("%s(%s)%s", e.Name, strings.Join(params, ", "), retStr)
}

func (e Method) typeNameString() string {
	params := make([]string, 0, len(e.Parameters))
	for _, param := range e.Parameters {
		params = append(params, param.String())
	}

	var retStr string
	if e.Return != TypeVoid {
		retStr = fmt.Sprintf(" %s", string(e.Return.GlobalName()))
	}

	return fmt.Sprintf("%s(%s)%s", e.Name, strings.Join(params, ", "), retStr)
}

func (e Method) Equal(o Method) bool {
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

func (e Method) BoundFunctionType() *FunctionType {
	return &FunctionType{
		Receiver:   TypeVoid,
		Parameters: e.Parameters,
		Return:     e.Return,
	}
}

func (e Method) UnboundFunction() *FunctionType {
	return &FunctionType{
		Receiver:   TypeVoid,
		Parameters: append([]Type{e.Receiver}, e.Parameters...),
		Return:     e.Return,
	}
}

type MethodSet []Method

func (m *MethodSet) Add(name string, recv Type, params []Type, ret Type) error {
	entry := Method{
		Name:       name,
		Receiver:   recv,
		Parameters: params,
		Return:     ret,
	}

	if slices.ContainsFunc(*m, entry.Equal) {
		return fmt.Errorf("method set already contains method %q", name)
	}

	*m = append(*m, entry)

	slices.SortStableFunc(*m, func(a, b Method) int {
		return cmp.Compare(a.Name, b.Name)
	})

	return nil
}

func (m MethodSet) Has(name string) bool {
	return slices.ContainsFunc(m, func(method Method) bool {
		return method.Name == name
	})
}

func (m MethodSet) Get(name string) (Method, bool) {
	index := slices.IndexFunc(m, func(method Method) bool {
		return method.Name == name
	})
	if index == -1 {
		return Method{}, false
	}

	return m[index], true
}

func (m MethodSet) Equal(o MethodSet) bool {
	return slices.EqualFunc(m, o, Method.Equal)
}

func (m MethodSet) Subset(o MethodSet) bool {
	for _, entry := range m {
		if !slices.ContainsFunc(o, entry.Equal) {
			return false
		}
	}

	return true
}

func TypeMethods(typ Type) MethodSet {
	switch typ := dereferenceType(typ).(type) {
	case *DerivedType:
		return typ.Methods(false)
	case *PointerType:
		switch typ := dereferenceType(typ.pointee).(type) {
		case *DerivedType:
			return typ.Methods(false)
		}
	}

	return nil
}

func TypeMethod(typ Type, name string) (*Function, bool) {
	switch typ := dereferenceType(typ).(type) {
	case *DerivedType:
		if typ.Methods(false).Has(name) {
			return typ.Method(name, false), true
		}

		return nil, false
	case *PointerType:
		switch typ := dereferenceType(typ.pointee).(type) {
		case *DerivedType:
			if typ.Methods(true).Has(name) {
				return typ.Method(name, true), true
			}

			return nil, false
		}
	}

	return nil, false
}

type DerivedType struct {
	name       string
	pkg        *Package
	methods    MethodSet
	ptrMethods MethodSet
	underlying Type

	methodFuncs    map[string]*Function
	ptrMethodFuncs map[string]*Function
}

func NewDerivedType(name string, pkg *Package, underlying Type) *DerivedType {
	return &DerivedType{
		name:       name,
		pkg:        pkg,
		underlying: underlying,

		methodFuncs:    make(map[string]*Function),
		ptrMethodFuncs: make(map[string]*Function),
	}

}

func (t *DerivedType) Name() string {
	return t.name
}

func (t *DerivedType) Package() *Package {
	return t.pkg
}

func (t *DerivedType) GlobalName() TypeName {
	if t.pkg == nil {
		return TypeName(fmt.Sprintf("%s", t.Name()))
	}
	return TypeName(fmt.Sprintf("%s.%s", t.pkg.QualifiedName(), t.Name()))
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

func (t *DerivedType) AddMethod(name string, f *Function) error {
	if t.Method(name, true) != nil {
		return fmt.Errorf("type %s already has a method %q", t, name)
	}

	params := f.Parameters()

	paramTypes := make([]Type, 0, len(params))
	for _, param := range params {
		paramTypes = append(paramTypes, param.Type())
	}

	if _, ok := f.receiver.Type().(*PointerType); ok {
		t.ptrMethodFuncs[name] = f
		t.ptrMethods.Add(name, f.receiver.Type(), paramTypes, f.Return())
	} else {
		ptrF := f.withPointerReceiver()
		t.methodFuncs[name] = f
		t.methods.Add(name, f.receiver.Type(), paramTypes, f.Return())

		t.ptrMethodFuncs[name] = ptrF
		t.ptrMethods.Add(name, ptrF.receiver.Type(), paramTypes, ptrF.Return())
	}

	return nil
}

func (t *DerivedType) Methods(ptr bool) MethodSet {
	if ptr {
		return t.ptrMethods
	} else {
		return t.methods
	}
}

func (t *DerivedType) Method(name string, ptr bool) *Function {
	if ptr {
		return t.ptrMethodFuncs[name]
	} else {
		return t.methodFuncs[name]
	}
}

func (t *DerivedType) MethodFunctions() []*Function {
	var funs []*Function
	for _, fun := range t.methodFuncs {
		funs = append(funs, fun)
	}

	slices.SortStableFunc(funs, func(a, b *Function) int {
		return cmp.Compare(a.Name(), b.Name())
	})

	return funs
}

func (t *DerivedType) PtrMethodFunctions() []*Function {
	var funs []*Function
	for _, fun := range t.ptrMethodFuncs {
		funs = append(funs, fun)
	}

	slices.SortStableFunc(funs, func(a, b *Function) int {
		return cmp.Compare(a.Name(), b.Name())
	})

	return funs
}

func (t *DerivedType) BoundMethodType(name string) (*FunctionType, bool) {
	f := t.methodFuncs[name]
	if f == nil {
		return nil, false
	}

	ftype := f.Type().(*FunctionType)

	ftype.Parameters = ftype.Parameters[1:]
	return ftype, true
}

type PointerType struct {
	pointee Type

	parser.Position
}

func NewPointerType(pointee Type) *PointerType {
	return &PointerType{pointee: pointee}
}

func (*PointerType) Kind() Kind { return KindPointer }
func (*PointerType) Size() Size { return 1 }

func (t *PointerType) String() string {
	return fmt.Sprintf("*%s", t.Pointee().String())
}

func (t *PointerType) GlobalName() TypeName {
	return TypeName(fmt.Sprintf("*%s", string(t.Pointee().GlobalName())))
}

func (t *PointerType) Pointee() Type {
	return t.pointee
}

type SliceType struct {
	elem Type

	parser.Position
}

func (t *SliceType) Kind() Kind { return KindSlice }

func (t *SliceType) String() string { return fmt.Sprintf("[]%s", t.elem.String()) }
func (t *SliceType) GlobalName() TypeName {
	return TypeName(fmt.Sprintf("[]%s", string(t.elem.GlobalName())))
}

func (t *SliceType) Elem() Type { return t.elem }
func (*SliceType) Size() Size   { return sliceHeader.Size() }

func (t *SliceType) AsVariadic() *VariadicType {
	return &VariadicType{elem: t.elem, Position: t.Position}
}

var sliceHeader = NewTupleType(
	NewPointerType(TypeVoid),
	TypeInt,
	TypeInt,
)

type ArrayType struct {
	length int
	elem   Type

	parser.Position
}

func (t *ArrayType) Kind() Kind { return KindArray }

func (t *ArrayType) String() string { return fmt.Sprintf("[%d]%s", t.length, t.elem.String()) }
func (t *ArrayType) GlobalName() TypeName {
	return TypeName(fmt.Sprintf("[%d]%s", t.length, string(t.elem.GlobalName())))
}

func (t *ArrayType) Elem() Type  { return t.elem }
func (t *ArrayType) Length() int { return t.length }
func (t *ArrayType) Size() Size  { return t.elem.Size() * Size(t.length) }

type TupleType struct {
	elems []Type

	parser.Position
}

func NewTupleType(elems ...Type) *TupleType {
	return &TupleType{elems: elems}
}

func (t *TupleType) Kind() Kind { return KindTuple }

func (t *TupleType) String() string {
	var strs []string
	for _, elem := range t.elems {
		strs = append(strs, elem.String())
	}

	return fmt.Sprintf("(%s)", strings.Join(strs, ", "))
}

func (t *TupleType) GlobalName() TypeName {
	var strs []string
	for _, elem := range t.elems {
		strs = append(strs, string(elem.GlobalName()))
	}

	return TypeName(fmt.Sprintf("(%s)", strings.Join(strs, ", ")))
}

func (t *TupleType) Elems() []Type { return t.elems }

func (t *TupleType) ElemOffset(index int) Size {
	var size Size
	for _, typ := range t.elems[:index] {
		size += typ.Size()
	}

	return size
}
func (t *TupleType) Size() Size {
	return t.ElemOffset(len(t.elems))
}

type MapType struct {
	key   Type
	value Type

	parser.Position
}

func (*MapType) Kind() Kind { return KindMap }

func (t *MapType) String() string {
	return fmt.Sprintf("map[%s]%s", t.key.String(), t.value.String())
}

func (t *MapType) GlobalName() TypeName {
	return TypeName(fmt.Sprintf("map[%s]%s", string(t.key.GlobalName()), string(t.value.GlobalName())))
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
	fields []StructField

	parser.Position
}

func (*StructType) Kind() Kind { return KindStruct }

func (t *StructType) String() string {
	var parts []string
	for _, entry := range t.fields {
		parts = append(parts, entry.String())
	}

	return fmt.Sprintf("struct{%s}", strings.Join(parts, "; "))
}

func (t *StructType) GlobalName() TypeName {
	var parts []string
	for _, entry := range t.fields {
		parts = append(parts, entry.typeNameString())
	}

	return TypeName(fmt.Sprintf("struct{%s}", strings.Join(parts, "; ")))
}

func (t *StructType) Fields() []StructField {
	return t.fields
}

func (t *StructType) GetField(name string) (StructField, bool) {
	index := slices.IndexFunc(t.fields, func(f StructField) bool {
		return f.Name == name
	})
	if index == -1 {
		return StructField{}, false
	}

	return t.fields[index], true
}

func (t *StructType) HasField(name string) bool {
	return slices.ContainsFunc(t.fields, func(f StructField) bool {
		return f.Name == name
	})
}

func (t *StructType) FieldOffset(name string) (Size, error) {
	index := slices.IndexFunc(t.fields, func(f StructField) bool {
		return f.Name == name
	})
	if index == -1 {
		return 0, fmt.Errorf("no such field %q", name)
	}

	var offset Size
	for _, f := range t.fields[:index] {
		offset += f.Type.Size()
	}

	return offset, nil
}

func (t *StructType) Size() Size {
	var offset Size
	for _, f := range t.fields {
		offset += f.Type.Size()
	}

	return offset
}

type StructField struct {
	Name string
	Type Type

	// TODO: maybe tags
	Tag string
}

func (f StructField) String() string {
	return fmt.Sprintf("%s %s", f.Name, f.Type)
}

func (f StructField) typeNameString() string {
	return fmt.Sprintf("%s %s", f.Name, string(f.Type.GlobalName()))
}

func (f StructField) IsExported() bool {
	return unicode.IsUpper([]rune(f.Name)[0])
}

type InterfaceType struct {
	methods MethodSet
}

func NewInterfaceType() *InterfaceType {
	return &InterfaceType{}
}

func (t *InterfaceType) With(name string, params []Type, ret Type) *InterfaceType {
	t.methods.Add(name, TypeVoid, params, ret)
	return t
}

func (t *InterfaceType) String() string {
	methods := t.Methods()

	var parts []string
	for _, entry := range methods {
		parts = append(parts, entry.String())
	}

	return fmt.Sprintf("interface{%s}", strings.Join(parts, "; "))
}

func (t *InterfaceType) GlobalName() TypeName {
	methods := t.Methods()

	var parts []string
	for _, entry := range methods {
		parts = append(parts, entry.typeNameString())
	}

	return TypeName(fmt.Sprintf("interface{%s}", strings.Join(parts, "; ")))
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
	TypeKind(KindType),
	NewPointerType(TypeVoid),
)

func (t *InterfaceType) ImplementedBy(i Type) bool {
	switch i := dereferenceType(i).(type) {
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

type TypeType struct {
	Type Type
}

func (*TypeType) Kind() Kind       { return KindType }
func (*TypeType) Size() Size       { return 1 }
func (t *TypeType) String() string { return fmt.Sprintf("Type(%s)", t.Type) }
func (t *TypeType) GlobalName() TypeName {
	return TypeName(fmt.Sprintf("Type(%s)", string(t.Type.GlobalName())))
}

type TypeExpression struct {
	typ Type

	parser.Position
}

func (t *TypeExpression) Type() Type {
	return &TypeType{Type: t.typ}
}

type TypeConversionType struct {
	Type Type
}

type InterfaceTypeCoercionExpression struct {
	Interface  *InterfaceType
	Expression Expression

	parser.Position
}

func (e *InterfaceTypeCoercionExpression) Type() Type {
	return e.Interface
}

func (e *InterfaceTypeCoercionExpression) WrapError(err error) error {
	return e.Expression.WrapError(err)
}

type ParenthesizedType struct {
	Type

	parser.Position
}

func (t *ParenthesizedType) String() string {
	return t.Type.String()
}

type VariadicType struct {
	elem Type

	parser.Position
}

func (t *VariadicType) String() string {
	return fmt.Sprintf("...%s", t.elem.String())
}

func (t *VariadicType) GlobalName() TypeName {
	return TypeName(fmt.Sprintf("...%s", string(t.elem.GlobalName())))
}

func (t *VariadicType) Size() Size {
	return sliceHeader.Size()
}

func (t *VariadicType) Kind() Kind {
	return KindVariadic
}

func (t *VariadicType) Elem() Type {
	return t.elem
}

func (t *VariadicType) AsSlice() *SliceType {
	return &SliceType{elem: t.elem, Position: t.Position}
}
