package compiler

import (
	"cmp"
	"fmt"
	"maps"
	"slices"

	"github.com/rhino1998/aeon/pkg/compiler/air"
	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/types"
)

type SymbolStub string

func (s SymbolStub) Name() string {
	return string(s)
}

type SymbolDependent interface {
	SymbolDependencies([]*SymbolReference) []*SymbolReference
}

type Symbol interface {
	Name() string
}

type SymbolReference struct {
	name  string
	scope *SymbolScope
}

func (e *SymbolReference) Dereference() Symbol {
	v, ok := e.scope.get(e.name)
	if !ok {
		return nil
	}

	return v
}

func (e *SymbolReference) Name() string {
	return e.name
}

func (s *SymbolReference) QualifiedName() string {
	return s.scope.qualifiedSymbolName(s.name)
}

type QualifiedSymbol interface {
	Symbol
	QualifiedName() string
}

type TypedSymbol interface {
	Symbol
	Type() types.Type
}

type SymbolScope struct {
	parent *SymbolScope
	name   string
	scope  map[string]Symbol

	prog       *Program
	pkg        *Package
	function   *Function
	errHandler Expression
}

func newScope(parent *SymbolScope, name string) *SymbolScope {
	return &SymbolScope{
		scope:  make(map[string]Symbol),
		name:   name,
		parent: parent,
	}
}

func (s *SymbolScope) Name() string {
	return s.name
}

func (s *SymbolScope) Parent() *SymbolScope {
	return s.parent
}

func (s *SymbolScope) qualifiedSymbolName(name string) string {
	if s.name == "" {
		return name
	}
	return s.name + "." + name
}

func (s *SymbolScope) SymbolNames() []string {
	if s == nil {
		return nil
	}

	ret := make([]string, 0, len(s.scope))
	ret = append(ret, slices.Collect(maps.Keys(s.scope))...)
	ret = append(ret, s.parent.SymbolNames()...)

	slices.Sort(ret)
	return ret
}

func (s *SymbolScope) Functions() []*Function {
	var funcs []*Function
	for _, val := range s.scope {
		switch val := val.(type) {
		case *Function:
			funcs = append(funcs, val)
		}
	}

	slices.SortFunc(funcs, func(a, b *Function) int {
		return cmp.Compare(a.Name(), b.Name())
	})

	return funcs
}

func (s *SymbolScope) Function() *Function {
	if s.function != nil {
		return s.function
	} else if s.parent == nil {
		return nil
	}

	return s.parent.Function()
}

func (s *SymbolScope) Packages() []*Package {
	var pkgs []*Package
	for _, val := range s.scope {
		switch val := val.(type) {
		case *Package:
			pkgs = append(pkgs, val)
		}
	}

	slices.SortFunc(pkgs, func(a, b *Package) int {
		return cmp.Compare(a.Name(), b.Name())
	})

	return pkgs
}

func (s *SymbolScope) ExternFunctions() []*ExternFunction {
	var externs []*ExternFunction
	for _, val := range s.scope {
		switch val := val.(type) {
		case *ExternFunction:
			externs = append(externs, val)
		}
	}

	slices.SortFunc(externs, func(a, b *ExternFunction) int {
		return cmp.Compare(a.Name(), b.Name())
	})

	return externs
}

func (s *SymbolScope) Program() *Program {
	if s.parent == nil {
		return s.prog
	}

	return s.parent.Program()
}

func (s *SymbolScope) Package() *Package {
	if s.pkg != nil {
		return s.pkg
	} else if s.parent == nil {
		return nil
	}

	return s.parent.Package()
}

func (s *SymbolScope) Variables() []*Variable {
	var funcs []*Variable
	for _, val := range s.scope {
		switch val := val.(type) {
		case *Variable:
			funcs = append(funcs, val)
		}
	}

	slices.SortFunc(funcs, func(a, b *Variable) int {
		return cmp.Compare(a.Name(), b.Name())
	})

	return funcs
}

func (s *SymbolScope) Constants() []*Constant {
	var funcs []*Constant
	for _, val := range s.scope {
		switch val := val.(type) {
		case *Constant:
			funcs = append(funcs, val)
		}
	}

	slices.SortFunc(funcs, func(a, b *Constant) int {
		return cmp.Compare(a.Name(), b.Name())
	})

	return funcs
}

func (s *SymbolScope) DerivedTypes() []*types.Derived {
	var funcs []*types.Derived
	for _, val := range s.scope {
		switch val := val.(type) {
		case *types.Derived:
			funcs = append(funcs, val)
		}
	}

	slices.SortFunc(funcs, func(a, b *types.Derived) int {
		return cmp.Compare(a.Name(), b.Name())
	})

	return funcs
}

func (s *SymbolScope) get(name string) (Symbol, bool) {
	if s == nil {
		var v Symbol
		return v, false
	}

	v, ok := s.scope[name]
	if ok {
		return v, true
	}

	return s.parent.get(name)
}

func (s *SymbolScope) put(symbol Symbol) error {
	name := symbol.Name()
	if maybeStub, ok := s.scope[name]; ok {
		if _, ok := maybeStub.(SymbolStub); !ok {
			return fmt.Errorf("%s is already defined in this scope", name)
		}
	}

	s.scope[name] = symbol

	return nil
}

func (s *SymbolScope) next() *SymbolScope {
	return newScope(s, "")
}

func (s *SymbolScope) ResolveType(name string) (types.Type, bool) {
	v, ok := s.get(name)
	if !ok {
		return nil, false
	}

	t, ok := v.(types.Type)
	if !ok {
		return nil, false
	}

	return t, true
}

func (s *SymbolScope) getPackage(name string) (*Package, bool) {
	v, ok := s.get(name)
	if !ok {
		return nil, false
	}

	p, ok := v.(*Package)
	if !ok {
		return nil, false
	}

	return p, true
}

func (s *SymbolScope) getFunction(name string) (*Function, bool) {
	v, ok := s.get(name)
	if !ok {
		return nil, false
	}

	f, ok := v.(*Function)
	if !ok {
		return nil, false
	}

	return f, true
}

func (s *SymbolScope) getTypedSymbol(name string) (TypedSymbol, bool) {
	v, ok := s.get(name)
	if !ok {
		return nil, false
	}

	t, ok := v.(TypedSymbol)
	if !ok {
		return nil, false
	}

	return t, true
}

type ValueScope struct {
	parent  *ValueScope
	name    string
	nextSub int

	function *Function
	symbols  *SymbolScope
	types    map[types.Name]types.Type
	strings  map[air.String]struct{}

	variables map[string]*air.Value

	usedRegisters map[air.Register]bool
	numRegisters  int
}

func NewValueScope(regs int, symbols *SymbolScope) *ValueScope {
	vs := &ValueScope{
		symbols:       symbols,
		function:      symbols.function,
		variables:     make(map[string]*air.Value),
		types:         make(map[types.Name]types.Type),
		strings:       make(map[air.String]struct{}),
		usedRegisters: make(map[air.Register]bool),
		numRegisters:  regs,
	}

	vs.getString("")

	for _, name := range symbols.SymbolNames() {
		vs.getString(air.String(name))
	}
	for _, typ := range symbols.DerivedTypes() {
		vs.registerType(typ)
	}

	return vs
}

func (vs *ValueScope) pkg(pkg *Package) *ValueScope {
	vs.nextSub++
	for _, name := range pkg.scope.SymbolNames() {
		vs.strings[air.String(name)] = struct{}{}
	}
	return &ValueScope{
		parent:  vs,
		symbols: pkg.scope,

		function:  pkg.scope.Function(),
		variables: maps.Clone(vs.variables),
		types:     vs.types,
		strings:   vs.strings,

		usedRegisters: maps.Clone(vs.usedRegisters),
		numRegisters:  vs.numRegisters,
	}
}

func (vs *ValueScope) sub(scope *SymbolScope) *ValueScope {
	vs.nextSub++
	for _, name := range scope.SymbolNames() {
		vs.strings[air.String(name)] = struct{}{}
	}
	return &ValueScope{
		parent:  vs,
		symbols: scope,

		function:  scope.Function(),
		variables: maps.Clone(vs.variables),
		types:     vs.types,
		strings:   vs.strings,

		usedRegisters: maps.Clone(vs.usedRegisters),
		numRegisters:  vs.numRegisters,
	}
}

func (vs *ValueScope) fun(scope *SymbolScope) *ValueScope {
	for _, name := range scope.SymbolNames() {
		vs.strings[air.String(name)] = struct{}{}
	}
	return &ValueScope{
		parent:  vs,
		symbols: scope,

		function:  scope.Function(),
		variables: maps.Clone(vs.variables),
		types:     vs.types,
		strings:   vs.strings,

		usedRegisters: maps.Clone(vs.usedRegisters),
		numRegisters:  vs.numRegisters,
	}
}

func (vs *ValueScope) newFunctionRef(fun *Function) *air.Value {
	return &air.Value{
		Type:    types.Kind(kinds.Int),
		Operand: air.LabelOperand(air.Label(fun.QualifiedName())),
	}
}

func (vs *ValueScope) newImmediate(imm air.Immediate) *air.Value {
	return &air.Value{
		Operand: air.ImmediateOperand(imm),
		Type:    types.Kind(imm.Kind()),
	}
}

func (vs *ValueScope) newConstant(name string, typ types.Type, op *air.Operand) {
	vs.variables[name] = &air.Value{
		Type:    typ,
		Operand: op,
	}
}

func (vs *ValueScope) newGlobal(v *Variable) *air.Value {
	if vs.parent != nil && vs.parent.pkg != nil {
		return vs.parent.newGlobal(v)
	}

	loc := &air.Value{
		Type:    types.NewPointer(types.Dereference(v.Type())),
		Operand: air.GlobalOperand(v),
	}

	globVal := loc.Value()

	vs.variables[v.Name()] = globVal

	vs.registerType(v.Type())

	return globVal
}

func (vs *ValueScope) newArg(name string, typ types.Type) *air.Value {
	val := &air.Value{
		Type:    typ,
		Operand: air.OperandRegisterSP.Dereference(),
	}
	vs.variables[name] = val
	return val
}

func (vs *ValueScope) newParam(name string, offset air.Size, typ types.Type) {
	vs.variables[name] = &air.Value{
		Type:    typ,
		Operand: air.OperandRegisterFP.AddConst(offset).Dereference(),
	}
}

func (vs *ValueScope) newLocal(v *Variable) *air.Value {
	loc := &air.Value{
		Type:    types.NewPointer(types.Dereference(v.Type())),
		Operand: air.LocalOperand(v),
	}

	locVal := loc.Value()

	vs.variables[v.Name()] = locVal

	vs.registerType(v.Type())

	return locVal
}

func (vs *ValueScope) allocTemp(typ types.Type) *air.Value {
	v := NewVariable("__local_tmp", typ, vs.symbols)

	tmp := &air.Value{
		Type:    types.NewPointer(types.Dereference(v.Type())),
		Operand: air.TempOperand(v),
	}

	tmpVal := tmp.Value()

	vs.variables[v.Name()] = tmpVal

	vs.registerType(v.Type())

	return tmpVal
}

func (vs *ValueScope) Get(name string) (*air.Value, bool) {
	if vs == nil {
		return nil, false
	}

	op, ok := vs.variables[name]
	if !ok {
		return vs.parent.Get(name)
	}

	return op, true
}

func (vs *ValueScope) typeName(typ types.Type) (*air.Value, error) {
	name, err := vs.registerType(typ)
	if err != nil {
		return nil, err
	}

	return &air.Value{
		Type:    types.Kind(kinds.Type),
		Operand: air.TypeOperand(name),
	}, nil
}

func (vs *ValueScope) Types() []types.Type {
	return sortedMapByKey(vs.types)
}

func (vs *ValueScope) registerType(typ types.Type) (types.Name, error) {
	if typ == types.Unknown {
		return "", fmt.Errorf("cannot register unknown type")
	}

	typ = types.Dereference(typ)
	name := typ.GlobalName()
	if other, ok := vs.types[name]; ok {
		if !types.Equal(typ, other) {
			return "", fmt.Errorf("duplicate non-equal types %s %s", typ, other)
		}

		return name, nil
	}

	vs.types[name] = typ
	vs.getString(air.String(name))
	vs.symbols.Program().registerType(typ)

	return name, nil
}

const (
	errorHandlerSymbolName = "#error_handler"
)

func (vs *ValueScope) getString(s air.String) *air.Value {
	vs.strings[s] = struct{}{}
	return &air.Value{
		Type:    types.Kind(kinds.String),
		Operand: air.StringOperand(s),
	}
}

func (vs *ValueScope) Strings() []air.String {
	return sortedMapKeysByKey(vs.strings)
}

func (vs *ValueScope) SP() *air.Value {
	return &air.Value{
		Type:    types.Kind(kinds.Int),
		Operand: air.OperandRegisterSP,
	}
}

func (vs *ValueScope) vtableLookup(typ *air.Value, method types.Method) *air.Value {
	return &air.Value{
		Type:    method.BoundFunction(),
		Operand: air.NewVTableLookup(typ.Operand, vs.getString(air.String(method.Name)).Operand),
	}
}
