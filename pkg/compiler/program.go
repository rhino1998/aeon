package compiler

import (
	"cmp"
	"fmt"
	"slices"
)

type Program struct {
	root *SymbolScope

	packages map[string]*Package
	types    map[TypeName]Type
	strings  map[String]struct{}

	registers int
	bytecode  BytecodeSnippet

	stringOffset Addr
}

func newProgram() *Program {
	p := &Program{
		root: BuiltinsSymbols(),

		packages:  make(map[string]*Package),
		types:     make(map[TypeName]Type),
		strings:   make(map[String]struct{}),
		registers: 0,
	}

	return p
}

func (p *Program) Registers() int {
	// always need at least pc, sp, fp
	return p.registers + 3
}

func (p *Program) Bytecode() []Bytecode {
	return p.bytecode
}

func (p *Program) FrameSize() Size {
	return Size(p.Registers())
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
	var externs []*ExternFunction

	externs = append(externs, p.root.ExternFunctions()...)

	for _, pkg := range p.packages {
		externs = append(externs, pkg.ExternFunctions()...)
	}

	slices.SortStableFunc(externs, func(a, b *ExternFunction) int {
		return cmp.Compare(a.Name(), b.Name())
	})

	return externs
}

func (p *Program) Types() []Type {
	return sortedMapByKey(p.types)
}

func (p *Program) Strings() []String {
	return sortedMapKeysByKey(p.strings)
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
		size += funcType.Size() * Size(len(drv.PtrMethodFunctions()))
	}

	return size
}

func (p *Program) registerType(t Type) {
	p.types[t.GlobalName()] = t
	p.strings[String(t.GlobalName())] = struct{}{}

}

func (p *Program) GlobalLayout() []TypeSlot {
	layout := make([]TypeSlot, 0)
	var offset Size

	addLayout := func(t Type) {
		layout = append(layout, TypeSlot{
			Offset: offset,
			Type:   t,
		})
		offset += t.Size()
	}

	for _, global := range p.Globals() {
		addLayout(global.Type())
	}

	for range p.Functions() {
		addLayout(funcType)
	}

	for range p.ExternFuncs() {
		addLayout(externType)
	}

	for _, drv := range p.DerivedTypes() {
		for range drv.MethodFunctions() {
			addLayout(funcType)
		}
		for range drv.PtrMethodFunctions() {
			addLayout(funcType)
		}
	}

	return layout
}

// TODO: topological sort by import
func (p *Program) Functions() []*Function {
	var ret []*Function
	for _, pkg := range p.Packages() {
		ret = append(ret, pkg.Functions()...)
	}

	return ret
}

func (p *Program) AllFunctions() []*Function {
	var ret []*Function
	for _, pkg := range p.Packages() {
		ret = append(ret, pkg.Functions()...)
	}

	for _, typ := range p.DerivedTypes() {
		for _, fun := range typ.MethodFunctions() {
			ret = append(ret, fun)
		}

		for _, fun := range typ.PtrMethodFunctions() {
			ret = append(ret, fun)
		}
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

	ret = append(ret, p.root.DerivedTypes()...)

	slices.SortStableFunc(ret, func(a, b *DerivedType) int {
		return cmp.Compare(a.GlobalName(), b.GlobalName())
	})

	return ret
}

type Package struct {
	name          string
	qualifiedName string
	prog          *Program
	scope         *SymbolScope
	values        *ValueScope

	imports []*Package

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

func (p *Package) Imports() []*Package {
	return p.imports
}

func (p *Package) AddImport(imp *Package) error {
	if slices.Contains(p.imports, p) {
		return nil
	}

	_, err := p.transitiveImports(nil, imp)
	if err != nil {
		return err
	}

	p.imports = append(p.imports, imp)

	slices.SortStableFunc(p.imports, func(a, b *Package) int {
		return cmp.Compare(a.QualifiedName(), b.QualifiedName())
	})

	return nil
}

func (p *Package) transitiveImports(stack []*Package, maybe *Package) (_ map[string]*Package, err error) {
	errs := newErrorSetWithWrapper(func(err error) error {
		return fmt.Errorf("from %s: %w", p.QualifiedName(), err)
	})
	defer func() {
		err = errs.Defer(err)
	}()

	if slices.Contains(stack, p) {
		errs.Add(fmt.Errorf("import cycle detected %s transitively imports %s", p.Name(), p.Name()))
	}

	stack = append(stack, p)
	seen := make(map[string]*Package)
	for _, imp := range p.imports {
		subSeen, err := imp.transitiveImports(stack, nil)
		if err != nil {
			errs.Add(err)
		}

		for name, pkg := range subSeen {
			seen[name] = pkg
		}
	}

	if maybe != nil {
		subSeen, err := maybe.transitiveImports(stack, nil)
		if err != nil {
			errs.Add(err)
		}

		for name, pkg := range subSeen {
			seen[name] = pkg
		}
	}

	return seen, nil
}

func (p *Package) TransitiveImports() []*Package {
	seen, _ := p.transitiveImports(nil, nil)
	return sortedMapByKey(seen)
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

func (p *Package) OffsetAddr(addr Addr) {
	p.addr = addr
	for _, fun := range p.Functions() {
		fun.OffsetAddr(addr)
	}

	for _, drv := range p.DerivedTypes() {
		for _, met := range drv.MethodFunctions() {
			met.OffsetAddr(addr)
		}

		for _, met := range drv.PtrMethodFunctions() {
			met.OffsetAddr(addr)
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

func (p *Package) VarInitFunction() *Function {
	return p.varinit
}

func (p *Package) InitFunctions() []*Function {
	return p.initFuncs
}

func (p *Package) UpdateFunctions() []*Function {
	return p.updateFuncs
}

func (p *Package) Strings() []String {
	return p.values.Strings()
}
