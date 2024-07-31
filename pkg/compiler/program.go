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
	scope := p.root
	var currentScope string
	parts := strings.Split(qualifiedName, ".")

	for len(parts) > 1 {
		sym, ok := scope.get(parts[0])
		if !ok {
			return nil, fmt.Errorf("no such package %q", qualifiedName)
		}

		switch sym := sym.(type) {
		case *Scope:
			scope = sym
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
		return nil, fmt.Errorf("symbol %q is not a function", qualifiedName)
	}
}
