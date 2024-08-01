package compiler

import (
	"fmt"
	"strings"
)

type Program struct {
	root *Scope
}

func newProgram() *Program {
	p := &Program{
		root: builtins(),
	}

	return p
}

func (p *Program) Functions() []*Function {
	return p.root.Functions()
}

func (p *Program) Function(qualifiedName string) (*Function, error) {
	return scopedFunction(p.root, qualifiedName)
}

type Package struct {
	name  string
	prog  *Program
	scope *Scope
}

func NewPackage(prog *Program, name string) *Package {
	return &Package{
		name:  name,
		prog:  prog,
		scope: newScope(prog.root, name),
	}
}

func (p *Package) Name() string {
	return p.name
}

func (p *Package) Functions() []*Function {
	return p.scope.Functions()
}

func (p *Package) Function(qualifiedName string) (*Function, error) {
	return scopedFunction(p.scope, qualifiedName)
}

func scopedFunction(scope *Scope, qualifiedName string) (*Function, error) {
	var currentScope string
	parts := strings.Split(qualifiedName, ".")

	for len(parts) > 1 {
		sym, ok := scope.get(parts[0])
		if !ok {
			return nil, fmt.Errorf("no such package %q", qualifiedName)
		}

		switch sym := sym.(type) {
		case *Package:
			scope = sym.scope
			currentScope += "." + parts[0]
			parts = parts[1:]
			continue
		default:
			return nil, fmt.Errorf("%q is not a package", qualifiedName)
		}
	}

	f, ok := scope.get(parts[0])
	if !ok {
		return nil, fmt.Errorf("no such function %q", qualifiedName)
	}

	switch f := f.(type) {
	case *Function:
		return f, nil
	default:
		return nil, fmt.Errorf("no such function %q", qualifiedName)
	}
}
