package compiler

type Program struct {
	root *SymbolScope

	externFuncs map[string]*ExternFunction
	packages    map[string]*Package
	types       map[TypeName]Type

	registers int
	bytecode  BytecodeSnippet
}

func newProgram() *Program {
	p := &Program{
		root: BuiltinsSymbols(),

		externFuncs: make(map[string]*ExternFunction),
		packages:    make(map[string]*Package),
		types:       make(map[TypeName]Type),
		registers:   16,
	}

	return p
}

func (p *Program) Registers() int {
	return p.registers
}

func (p *Program) Bytecode() []Bytecode {
	return p.bytecode
}

func (p *Program) FrameSize() Size {
	return Size(p.registers)
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

func (p *Program) Types() []Type {
	return sortedMapByKey(p.types)
}

func (p *Program) GlobalSize() Size {
	var size Size
	for _, global := range p.Globals() {
		size += global.Type().Size()
	}

	size += funcType.Size() * Size(len(p.Functions()))
	size += externType.Size() * Size(len(p.ExternFuncs()))

	for _, drv := range p.DerivedTypes() {
		size += funcType.Size() * Size(len(drv.MethodFunctions()))
	}

	return size
}

// TODO: topological sort by import
func (p *Program) Functions() []*Function {
	var ret []*Function
	for _, pkg := range p.Packages() {
		ret = append(ret, pkg.Functions()...)
	}

	return ret
}

// TODO: topological sort by import
func (p *Program) VarInitFunctions() []*Function {
	var ret []*Function
	for _, pkg := range p.Packages() {
		ret = append(ret, pkg.VarInitFunction())
	}

	return ret
}

// TODO: topological sort by import
func (p *Program) InitFunctions() []*Function {
	var ret []*Function
	for _, pkg := range p.Packages() {
		ret = append(ret, pkg.InitFunctions()...)
	}

	return ret
}

// TODO: topological sort by import
func (p *Program) UpdateFunctions() []*Function {
	var ret []*Function
	for _, pkg := range p.Packages() {
		ret = append(ret, pkg.UpdateFunctions()...)
	}

	return ret
}

// TODO: topological sort by import
func (p *Program) Globals() []*Variable {
	var ret []*Variable
	for _, pkg := range p.Packages() {
		ret = append(ret, pkg.Globals()...)
	}

	return ret
}

func (p *Program) DerivedTypes() []*DerivedType {
	var ret []*DerivedType
	for _, pkg := range p.Packages() {
		ret = append(ret, pkg.DerivedTypes()...)
	}

	return ret
}

type Package struct {
	name          string
	qualifiedName string
	prog          *Program
	scope         *SymbolScope
	values        *ValueScope

	varinit     *Function
	initFuncs   []*Function
	updateFuncs []*Function

	addr     Addr
	bytecode BytecodeSnippet
}

func NewPackage(prog *Program, name string) *Package {
	pkg := &Package{
		name:          name,
		qualifiedName: name, // TODO: compute by path/w/e
		prog:          prog,
		scope:         newScope(prog.root, name),
	}
	pkg.scope.pkg = pkg

	return pkg
}

func (p *Package) Name() string {
	return p.name
}

func (p *Package) QualifiedName() string {
	return p.qualifiedName
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

func (p *Package) KnownTypes() []Type {
	return p.values.Types()
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

	for _, drv := range p.DerivedTypes() {
		for _, met := range drv.MethodFunctions() {
			met.SetAddr(met.Addr() + addr)
		}
	}
}

func (p *Package) Globals() []*Variable {
	return p.scope.Variables()
}

func (p *Package) Constants() []*Constant {
	return p.scope.Constants()
}

func (p *Package) DerivedTypes() []*DerivedType {
	return p.scope.DerivedTypes()
}

func (p *Package) Imports() *Package {
	return nil
}

func (p *Package) VarInitFunction() *Function {
	return p.varinit
}

func (p *Package) InitFunctions() []*Function {
	return p.initFuncs
}

func (p *Package) UpdateFunctions() []*Function {
	return p.updateFuncs
}
