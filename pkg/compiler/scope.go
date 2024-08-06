package compiler

import (
	"cmp"
	"fmt"
	"log"
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

	for _, f := range funcs {
		log.Println(f.Name())
	}

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

func (s *SymbolScope) Package() *Package {
	if s.pkg != nil {
		return s.pkg
	} else if s.parent == nil {
		return nil
	}

	return s.parent.Package()
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

type ValueScope struct {
	parent *ValueScope

	function *Function
	symbols  *SymbolScope

	variables map[string]*Operand
	nextLocal AddrOffset
	maxLocal  *AddrOffset

	usedRegisters map[Register]bool
	numRegisters  int
}

func NewValueScope(regs int, symbols *SymbolScope) *ValueScope {
	return &ValueScope{
		symbols:       symbols,
		function:      symbols.function,
		variables:     make(map[string]*Operand),
		nextLocal:     1,
		maxLocal:      new(AddrOffset),
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

func (vs *ValueScope) newArg(offset AddrOffset) *Operand {
	op := &Operand{
		Kind: OperandKindIndirect,
		Value: Indirect{
			Base:   OperandRegisterSP,
			Offset: offset,
		},
	}
	vs.variables[fmt.Sprintf("arg_%d", int(offset))] = op
	return op
}

func (vs *ValueScope) newParam(name string, offset AddrOffset) {
	vs.variables[name] = &Operand{
		Kind: OperandKindIndirect,
		Value: Indirect{
			Base:   OperandRegisterFP,
			Offset: offset,
		},
	}
}

func (vs *ValueScope) newLocal(name string, typ Type) *Operand {
	o := &Operand{
		Kind: OperandKindIndirect,
		Value: Indirect{
			Base:   OperandRegisterFP,
			Offset: vs.nextLocal,
		},
	}

	vs.variables[name] = o

	if *vs.maxLocal < vs.nextLocal {
		*vs.maxLocal = vs.nextLocal
	}

	vs.nextLocal += AddrOffset(typ.Size())

	return o
}

func (vs *ValueScope) allocTemp(typ Type) *Operand {
	if typ.Size() == 1 {
		for reg := range Register(vs.numRegisters) {
			switch reg {
			case RegisterZero, RegisterFP, RegisterSP, RegisterReturn:
				continue
			default:
				if !vs.usedRegisters[reg] {
					vs.usedRegisters[reg] = true

					return &Operand{
						Kind:  OperandKindRegister,
						Value: reg,
					}
				}
			}
		}
	}

	// all registers are used or type is too large
	return vs.newLocal(fmt.Sprintf("__local_tmp_%d", int(vs.nextLocal)), typ)
}

func (vs *ValueScope) deallocTemp(o *Operand) {
	if o.Kind != OperandKindRegister {
		if o.Value.(Indirect).Offset == vs.nextLocal-1 {
			vs.nextLocal -= o.Value.(Indirect).Offset
		}

		return
	}

	vs.usedRegisters[o.Value.(Register)] = false
}

func (vs *ValueScope) Push(name string, typ Type, value *Operand) Mov {
	return Mov{
		Src: value,
		Dst: vs.newLocal(name, typ),
	}
}

func (vs *ValueScope) Get(name string) *Operand {
	return vs.variables[name]
}
