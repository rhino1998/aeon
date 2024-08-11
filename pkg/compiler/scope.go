package compiler

import (
	"cmp"
	"fmt"
	"maps"
	"slices"
)

type SymbolStub string

func (s SymbolStub) Name() string {
	return string(s)
}

type Symbol interface {
	Name() string
}

type TypedSymbol interface {
	Symbol
	Type() Type
}

type SymbolScope struct {
	parent *SymbolScope
	name   string
	scope  map[string]Symbol

	pkg      *Package
	function *Function
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

func (s *SymbolScope) Functions() []*Function {
	var funcs []*Function
	for _, val := range s.scope {
		switch val := val.(type) {
		case *Function:
			funcs = append(funcs, val)
		case *SymbolScope:
			funcs = append(funcs, val.Functions()...)
		case *Package:
			funcs = append(funcs, val.Functions()...)
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

func (s *SymbolScope) Types() []*DerivedType {
	var typs []*DerivedType
	for _, val := range s.scope {
		switch val := val.(type) {
		case *DerivedType:
			typs = append(typs, val)
		}
	}

	slices.SortFunc(typs, func(a, b *DerivedType) int {
		return cmp.Compare(a.Name(), b.Name())
	})

	return typs
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

func (s *SymbolScope) getType(name string) (Type, bool) {
	v, ok := s.get(name)
	if !ok {
		return nil, false
	}

	t, ok := v.(Type)
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

type LocationKind int

const (
	LocationKindConstant LocationKind = iota
	LocationKindRegister
	LocationKindGlobal
	LocationKindLocal
	LocationKindArg
	LocationKindParam
	LocationKindHeap
)

type Location struct {
	Kind LocationKind
	Name string
	Type Type
	*Operand
}

func (l *Location) AddressOf() *Location {
	return &Location{
		Kind:    l.Kind,
		Name:    fmt.Sprintf("&%s", l.Name),
		Type:    NewPointerType(l.Type),
		Operand: l.Operand.AddressOf(),
	}
}

func (l *Location) Dereference() (*Location, error) {
	typ := BaseType(l.Type).(*PointerType)

	return &Location{
		Kind: l.Kind,

		Name:    fmt.Sprintf("*%s", l.Name),
		Type:    typ.Pointee(),
		Operand: l.Operand.Dereference(),
	}, nil
}

func (l *Location) IndexTuple(index int) (*Location, error) {
	typ := BaseType(l.Type).(*TupleType)

	if index >= len(typ.Elems()) {
		return nil, fmt.Errorf("compile-time tuple index %d out of bounds", index)
	}

	return &Location{
		Kind:    l.Kind,
		Name:    fmt.Sprintf("%s.%d", l.Name, index),
		Type:    typ.Elems()[index],
		Operand: l.Operand.AddressOf().ConstOffset(typ.ElemOffset(index)).Dereference(),
	}, nil
}

func (l *Location) IndexArrayConst(index int) (*Location, error) {
	typ := BaseType(l.Type).(*ArrayType)

	if index >= typ.Length() {
		return nil, fmt.Errorf("compile-time array index %d out of bounds", index)
	}

	return &Location{
		Kind:    l.Kind,
		Name:    fmt.Sprintf("%s.%d", l.Name, index),
		Type:    typ.Elem(),
		Operand: l.Operand.AddressOf().ConstOffset(typ.Elem().Size() * Size(index)).Dereference(),
	}, nil
}

func (l *Location) IndexArray(index *Location) (*Location, error) {
	typ := BaseType(l.Type).(*ArrayType)

	return &Location{
		Kind:    l.Kind,
		Name:    fmt.Sprintf("%s.%d", l.Name, index),
		Type:    typ.Elem(),
		Operand: l.Operand.AddressOf().Offset(index.Operand.Stride(ImmediateOperand(Int(typ.Elem().Size())))).Dereference(),
	}, nil
}

func (l *Location) IndexSlice(index *Location) (*Location, error) {
	typ := BaseType(l.Type).(*SliceType)

	if index.Type.Kind() != KindInt {
		return nil, fmt.Errorf("invalid type for slice index %s", index.Type)
	}

	return &Location{
		Kind:    LocationKindHeap,
		Name:    fmt.Sprintf("%s[%s]", l.Name, index.Name),
		Type:    typ.Elem(),
		Operand: l.Operand.AddressOf().ConstOffset(2).Dereference().Offset(index.Operand).Dereference(),
	}, nil
}

type ValueScope struct {
	parent *ValueScope

	function *Function
	symbols  *SymbolScope

	nextGlobal Size

	variables map[string]*Location
	nextLocal Size
	maxLocal  *Size

	usedRegisters map[Register]bool
	numRegisters  int
}

func NewValueScope(regs int, symbols *SymbolScope) *ValueScope {
	return &ValueScope{
		symbols:       symbols,
		function:      symbols.function,
		variables:     make(map[string]*Location),
		nextGlobal:    1,
		nextLocal:     1,
		maxLocal:      new(Size),
		usedRegisters: make(map[Register]bool),
		numRegisters:  regs,
	}
}

func (vs *ValueScope) sub(scope *SymbolScope) *ValueScope {
	return &ValueScope{
		parent:  vs,
		symbols: scope,

		function:  scope.function,
		variables: maps.Clone(vs.variables),
		maxLocal:  vs.maxLocal,
		nextLocal: vs.nextLocal,

		usedRegisters: maps.Clone(vs.usedRegisters),
		numRegisters:  vs.numRegisters,
	}
}

func (vs *ValueScope) newFunctionRef(fun *Function) *Location {
	return &Location{
		Kind:    LocationKindConstant,
		Name:    fun.Name(),
		Type:    TypeKind(KindInt),
		Operand: fun.AddrOp(),
	}
}

func (vs *ValueScope) newImmediate(imm Immediate) *Location {
	return &Location{
		Kind:    LocationKindConstant,
		Operand: ImmediateOperand(imm),
		Type:    TypeKind(imm.Kind()),
	}
}

func (vs *ValueScope) newConstant(name string, typ Type, imm Immediate) {
	vs.variables[name] = &Location{
		Kind:    LocationKindConstant,
		Name:    name,
		Type:    typ,
		Operand: ImmediateOperand(imm),
	}
}

func (vs *ValueScope) newGlobal(name string, typ Type) *Location {
	if vs.parent != nil {
		return vs.parent.newGlobal(name, typ)
	}
	loc := &Location{
		Kind:    LocationKindGlobal,
		Name:    name,
		Type:    typ,
		Operand: ImmediateOperand(Int(vs.nextGlobal)).Dereference(),
	}

	vs.variables[name] = loc

	vs.nextGlobal += Size(typ.Size())
	return loc
}

func (vs *ValueScope) newArg(name string, offset Size, typ Type) *Location {
	loc := &Location{
		Kind:    LocationKindArg,
		Name:    fmt.Sprintf("arg_%s", name),
		Type:    typ,
		Operand: OperandRegisterSP.ConstOffset(offset).Dereference(),
	}
	vs.variables[loc.Name] = loc
	return loc
}

func (vs *ValueScope) newParam(name string, offset Size, typ Type) {
	vs.variables[name] = &Location{
		Name:    name,
		Kind:    LocationKindParam,
		Type:    typ,
		Operand: OperandRegisterFP.ConstOffset(offset).Dereference(),
	}
}

func (vs *ValueScope) newLocal(name string, typ Type) *Location {
	loc := &Location{
		Kind:    LocationKindLocal,
		Name:    name,
		Type:    typ,
		Operand: OperandRegisterFP.ConstOffset(vs.nextLocal).Dereference(),
	}

	vs.variables[name] = loc

	if *vs.maxLocal < vs.nextLocal {
		*vs.maxLocal = vs.nextLocal
	}

	vs.nextLocal += Size(typ.Size())

	return loc
}

func (vs *ValueScope) allocTemp(typ Type) *Location {
	if typ.Size() == 1 {
		for reg := range Register(vs.numRegisters) {
			switch reg {
			case RegisterZero, RegisterFP, RegisterSP:
				continue
			default:
				if !vs.usedRegisters[reg] {
					vs.usedRegisters[reg] = true

					return &Location{
						Kind: LocationKindRegister,
						Name: reg.String(),
						Type: typ,
						Operand: &Operand{
							Kind:  OperandKindRegister,
							Value: reg,
						},
					}
				}
			}
		}
	}

	// all registers are used or type is too large
	return vs.newLocal(fmt.Sprintf("__local_tmp_%d", int(vs.nextLocal)), typ)
}

func (vs *ValueScope) deallocTemp(l *Location) {
	switch l.Kind {
	case LocationKindLocal:
		offset := Size(l.Value.(Indirect).Ptr.Value.(Offset).B.Value.(Int))
		if offset == vs.nextLocal-l.Type.Size() {
			vs.nextLocal -= l.Type.Size()

		}
	case LocationKindRegister:
		vs.usedRegisters[l.Value.(Register)] = false
	}
}

func (vs *ValueScope) Push(name string, value *Location) Mov {
	return vs.Mov(value, vs.newLocal(name, value.Type))
}

func (vs *ValueScope) Mov(src, dst *Location) Mov {
	return Mov{
		Src:  src.Operand,
		Dst:  dst.Operand,
		Size: min(src.Type.Size(), dst.Type.Size()),
	}
}

func (vs *ValueScope) Get(name string) (*Location, bool) {
	op, ok := vs.variables[name]
	if !ok {
		return nil, false
	}

	return op, true
}
