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
		registers:   16,
	}

	return p
}

func (p *Program) Bytecode() []Bytecode {
	return p.bytecode
}

func (p *Program) Functions() []*Function {
	return p.root.Functions()
}

func (p *Program) FrameSize() AddrOffset {
	return AddrOffset(p.registers)
}

func (p *Program) AddPackage(name string) *Package {
	pkg, ok := p.packages[name]
	if ok {
		return pkg
	}

	pkg = NewPackage(p, name)

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

func (p *Program) GlobalSize() int {
	return 20
}

type Package struct {
	name  string
	prog  *Program
	scope *SymbolScope

	varinit *Function

	addr     Addr
	bytecode BytecodeSnippet
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

func (p *Package) ExternFunctions() []*ExternFunction {
	return p.scope.ExternFunctions()
}

func (p *Package) Addr() Addr {
	return p.addr
}

func (p *Package) Bytecode() BytecodeSnippet {
	return p.bytecode
}

func (p *Package) SetAddr(addr Addr) {
	p.addr = addr
	for _, fun := range p.Functions() {
		fun.SetAddr(fun.Addr() + addr)
	}

	if p.varinit != nil {
		p.varinit.SetAddr(p.varinit.Addr() + addr)
	}
}

func (p *Package) Globals() []*Variable {
	return p.scope.Variables()
}

func (p *Package) Constants() []*Constant {
	return p.scope.Constants()
}
