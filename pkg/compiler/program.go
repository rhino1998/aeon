package compiler

type Program struct {
	root *SymbolScope

	externFuncs map[string]*ExternFunction
	packages    map[string]*Package

	registers int
	bytecode  BytecodeSnippet
}

func newProgram() *Program {
	p := &Program{
		root: builtins(),

		externFuncs: make(map[string]*ExternFunction),
		packages:    make(map[string]*Package),
	}

	return p
}

func (p *Program) Bytecode() []Bytecode {
	return p.bytecode
}

func (p *Program) Functions() []*Function {
	return p.root.Functions()
}

func (p *Program) FrameSize() int {
	return p.registers
}

func (p *Program) AddPackage(name string) *Package {
	pkg := NewPackage(p, name)

	p.packages[name] = pkg

	return pkg
}

func (p *Program) Packages() []*Package {
	return sortedMap(p.packages, (*Package).Name)
}

func (p *Program) Package(name string) (*Package, bool) {
	pkg, ok := p.packages[name]
	return pkg, ok
}

func (p *Program) ExternFuncs() []*ExternFunction {
	return sortedMapByKey(p.externFuncs)
}

type Package struct {
	name  string
	prog  *Program
	scope *SymbolScope
}

func NewPackage(prog *Program, name string) *Package {
	pkg := &Package{
		name:  name,
		prog:  prog,
		scope: newScope(prog.root, name),
	}
	pkg.scope.pkg = pkg

	return pkg
}

func (p *Package) Name() string {
	return p.name
}

func (p *Package) Functions() []*Function {
	return p.scope.Functions()
}

func (p *Package) Function(name string) (*Function, bool) {
	return p.scope.getFunction(name)
}
