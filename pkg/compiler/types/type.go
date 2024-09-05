package types

import (
	"cmp"
	"fmt"
	"slices"
	"strings"
	"unicode"

	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/parser"
)

type TypeScope interface {
	ResolveType(name string) (Type, bool)
}

type Type interface {
	Kind() kinds.Kind
	String() string
	GlobalName() Name
}

var Unknown = Kind(kinds.Unknown)

var Void = Kind(kinds.Void)

var Nil = Kind(kinds.Nil)

var Discard = Kind(kinds.Discard)

var AnyPointer = NewPointer(Void)

func ConversionBound(t Type) Type {
	switch t := t.(type) {
	case *Derived:
		return t.Underlying()
	case *Reference:
		return t.Dereference()
	case *Pointer:
		return NewPointer(ConversionBound(t.Pointee()))
	case *Slice:
		return NewSlice(ConversionBound(t.Elem()))
	case *Array:
		// TODO: correct this for interfaces
		return NewArray(t.Length(), ConversionBound(t.Elem()))
	case *Tuple:
		elems := make([]Type, len(t.Elems()))
		for i, elem := range t.Elems() {
			elems[i] = ConversionBound(elem)
		}

		return NewTuple(elems...)
	case *Interface:
		return t
	case *Map:
		// TODO: maybe redo this
		return t
	default:
		return t
	}
}

func IsValidMethodReceiverType(t Type) bool {
	switch t.Kind() {
	case kinds.Interface, kinds.Void, kinds.Nil, kinds.Unknown:
		return false
	default:
	}

	switch t := Dereference(t).(type) {
	case *Derived:
		return true
	case *Pointer:
		switch Dereference(t.Pointee()).(type) {
		case *Derived:
			return true
		}

	}

	return false
}

func IsAssignableTo(v, to Type) bool {
	if v.Kind() == kinds.Type || v.Kind() == kinds.Builtin {
		return false
	}

	if IsUnspecified(to) {
		return true
	}

	if Equal(v, to) {
		return true
	}

	if v.Kind() == to.Kind() {
		if _, ok := to.(Kind); ok {
			return true
		}

		if _, ok := v.(Kind); ok {
			return true
		}
	}

	if v.Kind() == kinds.Nil {
		return true
	}

	if to, ok := Resolve(to).(*Interface); ok {
		return to.ImplementedBy(v)
	}

	if Equal(Resolve(v), Resolve(to)) {
		return true
	}

	return false
}

func IsConvertibleTo(v, to Type) bool {
	if IsAssignableTo(v, to) {
		return true
	}

	if v.Kind() == kinds.Type {
		return false
	}

	if v.Kind().IsNumeric() && to.Kind().IsNumeric() {
		return true
	}

	return Equal(Base(v), Base(to))
}

func Base(typ Type) Type {
	switch typ := Dereference(typ).(type) {
	case *Reference:
		return Base(typ.Dereference())
	case *Derived:
		return Base(typ.underlying)
	case *Pointer:
		return NewPointer(Base(typ.pointee))
	case *Array:
		return &Array{
			length: typ.length,

			elem: Base(typ.elem),
		}
	default:
		return typ
	}
}

func Dereference(typ Type) Type {
	switch typ := typ.(type) {
	case *Reference:
		res := Dereference(typ.Dereference())
		if res == Unknown {
			return typ
		}

		return res
	case *Parenthesized:
		return Dereference(typ.Type)
	default:
		return typ
	}
}

func Resolve(typ Type) Type {
	switch typ := typ.(type) {
	case *Reference:
		res := Resolve(typ.Dereference())
		if res == Unknown {
			return typ
		}

		return res
	case *Parenthesized:
		return Resolve(typ.Type)
	case *Derived:
		return Resolve(typ.underlying)
	default:
		return typ
	}
}

func equal(t1, t2 Type) bool {
	if t1.Kind() != t2.Kind() {
		return false
	}

	switch t1 := t1.(type) {
	case Kind:
		return t1 == t2 && t1.Kind() != kinds.Unknown
	case *Pointer:
		switch t2 := t2.(type) {
		case *Pointer:
			return Equal(t1.Pointee(), t2.Pointee())
		default:
			return false
		}
	case *Derived:
		return t1 == t2
	case *Variadic:
		switch t2 := t2.(type) {
		case *Variadic:
			return Equal(t1.Elem(), t2.Elem())
		default:
			return false
		}
	case *Slice:
		switch t2 := t2.(type) {
		case *Slice:
			return Equal(t1.Elem(), t2.Elem())
		default:
			return false
		}
	case *Map:
		switch t2 := t2.(type) {
		case *Map:
			return Equal(t1.Key(), t2.Key()) && Equal(t1.Value(), t2.Value())
		default:
			return false
		}
	case *Array:
		switch t2 := t2.(type) {
		case *Array:
			return t1.Length() == t2.Length() && Equal(t1.Elem(), t2.Elem())
		default:
			return false
		}
	case *Tuple:
		switch t2 := t2.(type) {
		case *Tuple:
			return slices.EqualFunc(t1.Elems(), t2.Elems(), Equal)
		default:
			return false
		}
	case *Function:
		switch t2 := t2.(type) {
		case *Function:
			if !Equal(t1.Receiver, t2.Receiver) {
				return false
			}

			if !slices.EqualFunc(t1.Parameters, t2.Parameters, Equal) {
				return false
			}

			if !Equal(t1.Receiver, t2.Receiver) {
				return false
			}

			if !Equal(t1.Return, t2.Return) {
				return false
			}

			return true
		default:
			return false
		}
	case *Struct:
		switch t2 := t2.(type) {
		case *Struct:
			t1f := t1.Fields()
			t2f := t2.Fields()
			if len(t1f) != len(t2f) {
				return false
			}

			for i := range len(t1f) {
				if !Equal(t1f[i].Type, t2f[i].Type) {
					return false
				}
			}

			return true
		default:
			return false
		}
	case *Interface:
		switch t2 := t2.(type) {
		case *Interface:
			return t1.methods.Equal(t2.methods)
		default:
			return false
		}
	default:
		return false
	}
}

func Equal(t1, t2 Type) bool {
	return equal(Dereference(t1), Dereference(t2))
}

func IsUnspecified(typ Type) bool {
	if typ == nil || typ == Unknown || typ == Discard {
		return true
	}

	_, ok := Dereference(typ).(Kind)
	return ok
}

func IsResolvable(typ Type) bool {
	switch typ := Dereference(typ).(type) {
	case Kind:
		return typ.Kind() != kinds.Unknown
	case *Pointer:
		return IsResolvable(typ.Pointee())
	case *Derived:
		return IsResolvable(typ.Underlying())
	case *Slice:
		return IsResolvable(typ.Elem())
	case *Array:
		return IsResolvable(typ.Elem())
	case *Map:
		return IsResolvable(typ.Key()) && IsResolvable(typ.Value())
	case *Variadic:
		return IsResolvable(typ.Elem())
	case *Tuple:
		for _, subType := range typ.Elems() {
			if !IsResolvable(subType) {
				return false
			}
		}

		return true
	case *Function:
		for _, param := range typ.Parameters {
			if !IsResolvable(param) {
				return false
			}
		}

		if !IsResolvable(typ.Receiver) {
			return false
		}

		if !IsResolvable(typ.Return) {
			return false
		}

		return true
	case *Interface:
		for _, method := range typ.Methods() {
			for _, param := range method.Parameters {
				if !IsResolvable(param) {
					return false
				}
			}

			if !IsResolvable(method.Return) {
				return false
			}
		}

		return true
	case *Struct:
		// TODO: handle self-pointers
		return true
		for _, field := range typ.Fields() {
			if !IsResolvable(field.Type) {
				return false
			}
		}

		return true
	default:
		return false
	}
}

type Kind kinds.Kind

func (t Kind) String() string {
	return string(t.GlobalName())
}

func (t Kind) GlobalName() Name {
	return Name(fmt.Sprintf("<kind %s>", t.Kind()))
}

func (t Kind) Kind() kinds.Kind {
	return kinds.Kind(t)
}

type Reference struct {
	s    TypeScope
	name string
	pkg  string
}

func NewReference(s TypeScope, name string) *Reference {
	return &Reference{s: s, name: name}
}

func (t Reference) Kind() kinds.Kind {
	return t.Dereference().Kind()
}

func (t Reference) Dereference() Type {
	ref, ok := t.s.ResolveType(t.name)
	if !ok {
		return Unknown
	}

	return ref
}

func (t Reference) String() string {
	return t.name
}

func (t Reference) GlobalName() Name {
	return t.Dereference().GlobalName()
}

type Method struct {
	Name       string
	Receiver   Type
	Parameters []Type
	Return     Type
}

func (m Method) QualifiedName() string {
	switch typ := Dereference(m.Receiver).(type) {
	case *Derived:
		return typ.MethodQualifiedName(false, m.Name)
	case *Pointer:
		switch typ := Dereference(typ.Pointee()).(type) {
		case *Derived:
			return typ.MethodQualifiedName(true, m.Name)
		default:
		}
	}

	panic(fmt.Sprintf("bug: invalid receiver type: %T", m.Receiver))
}

func (e Method) BoundType() *Function {
	return &Function{
		Receiver:   Void,
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
	if e.Return != Void {
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
	if e.Return != Void {
		retStr = fmt.Sprintf(" %s", string(e.Return.GlobalName()))
	}

	return fmt.Sprintf("%s(%s)%s", e.Name, strings.Join(params, ", "), retStr)
}

func (e Method) Equal(o Method) bool {
	if e.Name != o.Name {
		return false
	}

	if !slices.EqualFunc(e.Parameters, o.Parameters, Equal) {
		return false
	}

	if !Equal(e.Return, o.Return) {
		return false
	}

	return true
}

func (e Method) BoundFunction() *Function {
	return &Function{
		Receiver:   Void,
		Parameters: e.Parameters,
		Return:     e.Return,
	}
}

func (e Method) UnboundFunction() *Function {
	return &Function{
		Receiver:   Void,
		Parameters: append([]Type{e.Receiver}, e.Parameters...),
		Return:     e.Return,
	}
}

type MethodSet []Method

func (m *MethodSet) Add(name string, recv Type, params []Type, ret Type) error {
	entry := Method{
		Name:       name,
		Receiver:   Dereference(recv),
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

func Methods(typ Type) MethodSet {
	switch typ := Dereference(typ).(type) {
	case *Derived:
		return typ.Methods(false)
	case *Pointer:
		switch typ := Dereference(typ.pointee).(type) {
		case *Derived:
			return typ.Methods(true)
		}
	}

	return nil
}

func MethodsFuncName(typ Type, name string) (string, bool) {
	switch typ := Dereference(typ).(type) {
	case *Derived:
		return typ.MethodQualifiedName(false, name), true
	case *Pointer:
		switch typ := Dereference(typ.pointee).(type) {
		case *Derived:
			return typ.MethodQualifiedName(true, name), true
		}
	}

	return "", false
}

type Derived struct {
	name       string
	pkg        Namespace
	methods    MethodSet
	ptrMethods MethodSet
	underlying Type

	parser.Position
}

func NewDerived(name string, pkg Namespace, underlying Type) *Derived {
	return &Derived{
		name:       name,
		pkg:        pkg,
		underlying: underlying,
	}

}

func (t *Derived) NamespaceQualifiedName() string {
	if t.pkg == nil {
		return ""
	}
	return t.pkg.QualifiedName()
}

func (t *Derived) Name() string {
	return t.name
}

func (t *Derived) GlobalName() Name {
	if t.pkg == nil {
		return Name(fmt.Sprintf("%s", t.Name()))
	}

	return Name(fmt.Sprintf("%s.%s", t.pkg.QualifiedName(), t.Name()))
}

func (t *Derived) Kind() kinds.Kind {
	return t.underlying.Kind()
}

func (t *Derived) String() string {
	return t.name
}

func (t *Derived) Underlying() Type { return t.underlying }

func (t *Derived) SetUnderlying(typ Type) {
	t.underlying = typ
}

func (t *Derived) AddMethod(name string, f *Function) error {
	if t.methods.Has(name) || t.ptrMethods.Has(name) {
		return fmt.Errorf("type %s already has a method %q", t, name)
	}

	params := f.Parameters

	paramTypes := make([]Type, 0, len(params))
	for _, param := range params {
		paramTypes = append(paramTypes, param)
	}

	if _, ok := f.Receiver.(*Pointer); ok {
		t.ptrMethods.Add(name, f.Receiver, paramTypes, f.Return)
	} else {
		t.methods.Add(name, f.Receiver, paramTypes, f.Return)
		t.ptrMethods.Add(name, NewPointer(f.Receiver), paramTypes, f.Return)
	}

	return nil
}
func (t *Derived) Methods(ptr bool) MethodSet {
	if ptr {
		return t.ptrMethods
	} else {
		return t.methods
	}
}

func (t *Derived) MethodQualifiedName(ptr bool, name string) string {
	var namespace string
	if t.pkg != nil {
		namespace = t.pkg.QualifiedName() + "."
	}
	if ptr {
		return fmt.Sprintf("%s%s.%s", namespace, string(NewPointer(t).String()), name)
	} else {
		return fmt.Sprintf("%s%s.%s", namespace, string(t.String()), name)
	}
}

//
// func (t *DerivedType) MethodFunctions() []*Function {
// 	var funs []*Function
// 	for _, fun := range t.methodFuncs {
// 		funs = append(funs, fun)
// 	}
//
// 	slices.SortStableFunc(funs, func(a, b *Function) int {
// 		return cmp.Compare(a.Name(), b.Name())
// 	})
//
// 	return funs
// }
//
// func (t *DerivedType) PtrMethodFunctions() []*Function {
// 	var funs []*Function
// 	for _, fun := range t.ptrMethodFuncs {
// 		funs = append(funs, fun)
// 	}
//
// 	slices.SortStableFunc(funs, func(a, b *Function) int {
// 		return cmp.Compare(a.Name(), b.Name())
// 	})
//
// 	return funs
// }
//
// func (t *DerivedType) BoundMethodType(name string) (*FunctionType, bool) {
// 	f := t.methodFuncs[name]
// 	if f == nil {
// 		return nil, false
// 	}
//
// 	ftype := f.Type().(*FunctionType)
//
// 	ftype.Parameters = ftype.Parameters[1:]
// 	return ftype, true
// }

type Pointer struct {
	pointee Type

	parser.Position
}

func NewPointer(pointee Type) *Pointer {
	return &Pointer{pointee: pointee}
}

func (*Pointer) Kind() kinds.Kind { return kinds.Pointer }

func (t *Pointer) String() string {
	return fmt.Sprintf("*%s", t.Pointee().String())
}

func (t *Pointer) GlobalName() Name {
	return Name(fmt.Sprintf("*%s", string(t.Pointee().GlobalName())))
}

func (t *Pointer) Pointee() Type {
	return t.pointee
}

type Slice struct {
	elem Type

	parser.Position
}

func NewSlice(elem Type) *Slice {
	return &Slice{elem: elem}
}

func (t *Slice) Kind() kinds.Kind { return kinds.Slice }

func (t *Slice) String() string { return fmt.Sprintf("[]%s", t.elem.String()) }
func (t *Slice) GlobalName() Name {
	return Name(fmt.Sprintf("[]%s", string(t.elem.GlobalName())))
}

func (t *Slice) Elem() Type { return t.elem }

func (t *Slice) AsVariadic() *Variadic {
	return &Variadic{elem: t.elem, Position: t.Position}
}

type Array struct {
	length int
	elem   Type

	parser.Position
}

func NewArray(length int, elem Type) *Array {
	return &Array{length: length, elem: elem}
}

func (t *Array) Kind() kinds.Kind { return kinds.Array }

func (t *Array) String() string {
	return fmt.Sprintf("[%d]%s", t.length, t.elem.String())
}
func (t *Array) GlobalName() Name {
	return Name(fmt.Sprintf("[%d]%s", t.length, string(t.elem.GlobalName())))
}

func (t *Array) Elem() Type  { return t.elem }
func (t *Array) Length() int { return t.length }
func (t *Array) SetLength(length int) {
	t.length = length
}

type Tuple struct {
	elems []Type

	parser.Position
}

func NewTuple(elems ...Type) *Tuple {
	return &Tuple{elems: elems}
}

func (t *Tuple) Kind() kinds.Kind { return kinds.Tuple }

func (t *Tuple) String() string {
	var strs []string
	for _, elem := range t.elems {
		strs = append(strs, elem.String())
	}

	return fmt.Sprintf("(%s)", strings.Join(strs, ", "))
}

func (t *Tuple) GlobalName() Name {
	var strs []string
	for _, elem := range t.elems {
		strs = append(strs, string(elem.GlobalName()))
	}

	return Name(fmt.Sprintf("(%s)", strings.Join(strs, ", ")))
}

func (t *Tuple) Elems() []Type { return t.elems }

type Map struct {
	key   Type
	value Type

	parser.Position
}

func NewMap(key, value Type) *Map {
	return &Map{key: key, value: value}
}

func (*Map) Kind() kinds.Kind { return kinds.Map }

func (t *Map) String() string {
	return fmt.Sprintf("map[%s]%s", t.key.String(), t.value.String())
}

func (t *Map) GlobalName() Name {
	return Name(fmt.Sprintf("map[%s]%s", string(t.key.GlobalName()), string(t.value.GlobalName())))
}

func (t *Map) Key() Type {
	return t.key
}

func (t *Map) Value() Type {
	return t.value
}

type Struct struct {
	fields []StructField

	parser.Position
}

func NewStruct() *Struct {
	return &Struct{}
}

func (t *Struct) With(name string, typ Type) *Struct {
	t.fields = append(t.fields, StructField{Name: name, Type: typ})
	return t
}

func (*Struct) Kind() kinds.Kind { return kinds.Struct }

func (t *Struct) String() string {
	var parts []string
	for _, entry := range t.fields {
		parts = append(parts, entry.String())
	}

	return fmt.Sprintf("struct{%s}", strings.Join(parts, "; "))
}

func (t *Struct) GlobalName() Name {
	var parts []string
	for _, entry := range t.fields {
		parts = append(parts, entry.typeNameString())
	}

	return Name(fmt.Sprintf("struct{%s}", strings.Join(parts, "; ")))
}

func (t *Struct) Fields() []StructField {
	return t.fields
}

func (t *Struct) GetField(name string) (StructField, bool) {
	index := slices.IndexFunc(t.fields, func(f StructField) bool {
		return f.Name == name
	})
	if index == -1 {
		return StructField{}, false
	}

	return t.fields[index], true
}

func (t *Struct) HasField(name string) bool {
	return slices.ContainsFunc(t.fields, func(f StructField) bool {
		return f.Name == name
	})
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

type Interface struct {
	methods MethodSet
}

func NewInterface() *Interface {
	return &Interface{}
}

func (t *Interface) With(name string, params []Type, ret Type) *Interface {
	t.methods.Add(name, Void, params, ret)
	return t
}

func (t *Interface) String() string {
	methods := t.Methods()

	var parts []string
	for _, entry := range methods {
		parts = append(parts, entry.String())
	}

	return fmt.Sprintf("interface{%s}", strings.Join(parts, "; "))
}

func (t *Interface) GlobalName() Name {
	methods := t.Methods()

	var parts []string
	for _, entry := range methods {
		parts = append(parts, entry.typeNameString())
	}

	return Name(fmt.Sprintf("interface{%s}", strings.Join(parts, "; ")))
}

func (t *Interface) Methods() MethodSet {
	return t.methods
}

func (t *Interface) Kind() kinds.Kind {
	return kinds.Interface
}

func (t *Interface) ImplementedBy(i Type) bool {
	switch i := Dereference(i).(type) {
	case *Derived:
		if i.Underlying().Kind() == kinds.Interface {
			return t.ImplementedBy(i.Underlying())
		}
		return t.Methods().Subset(i.Methods(false))
	case *Pointer:
		switch pointee := Dereference(i.Pointee()).(type) {
		case *Derived:
			return t.Methods().Subset(pointee.Methods(true))
		default:
			return len(t.Methods()) == 0
		}
	case *Interface:
		return t.Methods().Subset(i.Methods())
	default:
		return len(t.Methods()) == 0
	}
}

type TypeType struct {
	Type Type
}

func (*TypeType) Kind() kinds.Kind { return kinds.Type }
func (t *TypeType) String() string { return fmt.Sprintf("Type(%s)", t.Type) }
func (t *TypeType) GlobalName() Name {
	return Name(fmt.Sprintf("Type(%s)", string(t.Type.GlobalName())))
}

type TypeConversionType struct {
	Type Type
}

type Parenthesized struct {
	Type

	parser.Position
}

func (t *Parenthesized) String() string {
	return t.Type.String()
}

func HasPointer(t Type) bool {
	switch t := t.(type) {
	case *Reference:
		return HasPointer(t.Dereference())
	case *Derived:
		return HasPointer(t.Underlying())
	case *Parenthesized:
		return HasPointer(t.Type)
	case Kind:
		switch t.Kind() {
		case kinds.String:
			return true
		default:
			return false
		}
	case *Pointer:
		return true
	case *Array:
		return HasPointer(t.Elem())
	case *Slice:
		return true
	case *Variadic:
		return true
	case *Tuple:
		for _, elem := range t.Elems() {
			if HasPointer(elem) {
				return true
			}
		}
		return false
	case *Struct:
		for _, field := range t.Fields() {
			if HasPointer(field.Type) {
				return true
			}
		}

		return false
	case *Map:
		return false // TODO:
	case *Function:
		return false
	case *Interface:
		return true
	default:
		panic(fmt.Sprintf("bug: unhandled type: %T", t))
	}
}
