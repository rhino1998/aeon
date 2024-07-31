package interpreter

import "fmt"

type Value interface{}

type Scope struct {
	parent *Scope
	name   string
	scope  map[string]Value
}

func newScope(parent *Scope, name string) *Scope {
	return &Scope{
		scope:  make(map[string]Value),
		name:   name,
		parent: parent,
	}
}

func (s *Scope) Get(name string) (Value, bool) {
	if s == nil {
		var v Value
		return v, false
	}

	v, ok := s.scope[name]
	if ok {
		return v, true
	}

	return s.parent.Get(name)
}

func (s *Scope) Put(name string, value Value) error {
	_, ok := s.scope[name]
	if ok {
		return fmt.Errorf("name %s already exists", name)
	}

	s.scope[name] = value
	return nil
}
