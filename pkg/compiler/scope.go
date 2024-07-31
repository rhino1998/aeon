package compiler

import "fmt"

type Symbol interface {
	Name() string
}

type TypedSymbol interface {
	Symbol
	Type() Type
}

type Scope struct {
	parent *Scope
	name   string
	scope  map[string]Symbol

	function *Function
}

func newScope(parent *Scope, name string) *Scope {
	return &Scope{
		scope:  make(map[string]Symbol),
		name:   name,
		parent: parent,
	}
}

func (s *Scope) Name() string {
	return s.name
}

func (s *Scope) Functions() []*Function {
	var funcs []*Function
	for _, val := range s.scope {
		switch val := val.(type) {
		case *Function:
			funcs = append(funcs, val)
		case *Scope:
			funcs = append(funcs, val.Functions()...)
		}
	}

	return funcs
}

func (s *Scope) Function() *Function {
	if s.function != nil {
		return s.function
	} else if s.parent == nil {
		return nil
	}

	return s.parent.Function()
}

func (s *Scope) get(name string) (Symbol, bool) {
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

func (s *Scope) put(symbol Symbol) error {
	name := symbol.Name()
	if _, ok := s.scope[name]; ok {
		return fmt.Errorf("%s is already defined in this scope", name)
	}

	s.scope[name] = symbol

	return nil
}

func (s *Scope) getType(name string) (Type, bool) {
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

func (s *Scope) getPackage(name string) (*Package, bool) {
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

func (s *Scope) getTypedSymbol(name string) (TypedSymbol, bool) {
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
