package compiler

import (
	"cmp"
	"fmt"
	"slices"

	"github.com/rhino1998/aeon/pkg/compiler/air"
	"github.com/rhino1998/aeon/pkg/compiler/types"
)

type Program struct {
	root   *SymbolScope
	values *ValueScope

	packages map[string]*Package
	types    map[types.Name]types.Type
	strings  map[air.String]struct{}

	globalSize   air.Size
	globalLayout []types.Type

	registers int
	bytecode  air.Snippet
}

func newProgram() *Program {
	scope := BuiltinsSymbols()
	p := &Program{
		root: scope,

		packages:  make(map[string]*Package),
		types:     make(map[types.Name]types.Type),
		strings:   make(map[air.String]struct{}),
		registers: 0,
	}
	scope.prog = p

	return p
}

func (p *Program) Registers() int {
	// always need at least pc, sp, fp
	return p.registers + 3
}

func (p *Program) Instructions() air.Snippet {
	return p.bytecode
}

func (p *Program) FrameSize() air.Size {
	return air.Size(p.Registers())
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

func (p *Program) Types() []types.Type {
	return sortedMapByKey(p.types)
}

func (p *Program) Strings() []air.String {
	return sortedMapKeysByKey(p.strings)
}

func (p *Program) GlobalLayout() []types.Type {
	return p.globalLayout
}

func (p *Program) GlobalSize() air.Size {
	return p.globalSize
}

func (p *Program) registerType(t types.Type) {
	if _, ok := p.types[t.GlobalName()]; ok {
		return
	}

	p.types[t.GlobalName()] = t
	p.strings[air.String(t.GlobalName())] = struct{}{}

	size, err := air.TypeSize(t)
	if err != nil {
		panic("bug: unresolvable type size: " + err.Error())
	}

	if size > 1 {
		ptrType := types.NewPointer(t)
		p.registerType(ptrType)
	}

	switch t := types.Dereference(t).(type) {
	case *types.Derived:
		p.registerType(t.Underlying())
	case *types.Pointer:
		p.registerType(t.Pointee())
	case *types.Array:
		p.registerType(t.Elem())
	case *types.Slice:
		p.registerType(t.Elem())
	case *types.Variadic:
		p.registerType(t.Elem())
	case *types.Tuple:
		for _, elem := range t.Elems() {
			p.registerType(elem)
		}
	case *types.Struct:
		for _, field := range t.Fields() {
			p.registerType(field.Type)
		}
	}
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
		var _ = typ
		// TODO: resolve method functions
		// for _, fun := range typ.MethodFunctions() {
		// 	ret = append(ret, fun)
		// }
		//
		// for _, fun := range typ.PtrMethodFunctions() {
		// 	ret = append(ret, fun)
		// }
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

	ret = append(ret, p.root.Variables()...)

	for _, pkg := range p.Packages() {
		ret = append(ret, pkg.Globals()...)
	}

	return ret
}

func (p *Program) DerivedTypes() []*types.Derived {
	var ret []*types.Derived
	for _, pkg := range p.Packages() {
		ret = append(ret, pkg.DerivedTypes()...)
	}

	ret = append(ret, p.root.DerivedTypes()...)

	slices.SortStableFunc(ret, func(a, b *types.Derived) int {
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

	addr     air.Addr
	bytecode air.Snippet
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

func (p *Package) KnownTypes() []types.Type {
	return p.values.Types()
}

func (p *Package) Instructions() air.Snippet {
	return p.bytecode
}

func (p *Package) OffsetAddr(addr air.Addr) {
	p.addr = addr
	for _, fun := range p.Functions() {
		fun.OffsetAddr(addr)
	}
}

func (p *Package) Globals() []*Variable {
	return p.scope.Variables()
}

func (p *Package) Constants() []*Constant {
	return p.scope.Constants()
}

func (p *Package) DerivedTypes() []*types.Derived {
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

func (p *Package) Strings() []air.String {
	return p.values.Strings()
}

func (p *Package) HeapAllocsAddr() air.Addr {
	val, _ := p.values.Get(heapAllocSliceName)
	return air.Addr(val.Operand.Value.(air.Int))
}

func (p *Package) HeapStrAllocsAddr() air.Addr {
	val, _ := p.values.Get(heapStrAllocSliceName)
	return air.Addr(val.Operand.Value.(air.Int))
}
