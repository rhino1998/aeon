package compiler

import (
	"context"
	"fmt"
	"strings"

	"github.com/rhino1998/aeon/pkg/parser"
)

var (
	TypeInt             = NewDerivedType("int", nil, TypeKind(KindInt))
	TypeString          = NewDerivedType("string", nil, TypeKind(KindString))
	TypeBool            = NewDerivedType("bool", nil, TypeKind(KindBool))
	TypeFloat           = NewDerivedType("float", nil, TypeKind(KindFloat))
	TypeAny             = NewDerivedType("any", nil, NewInterfaceType())
	TypeError           = NewDerivedType("error", nil, NewInterfaceType().With("Error", nil, TypeString))
	BuiltinExternAssert = &ExternFunction{
		name: "__builtin_assert",
		parameters: []Type{
			TypeKind(KindBool),
			TypeKind(KindString),
		},
		ret: TypeVoid,
	}

	BuiltinExternAppend1 = &ExternFunction{
		name: "__builtin_append1",
		parameters: []Type{
			TypeKind(KindPointer),
			TypeKind(KindInt),
			TypeKind(KindInt),
			TypeKind(KindPointer),
			TypeKind(KindInt),
			TypeKind(KindPointer),
		},
		ret: TypeVoid,
	}
)

var (
	Nil = &Constant{
		name:               "nil",
		typ:                NilType,
		ConstantExpression: NewLiteral(NilValue),
	}
)

var (
	BuiltinLen = &BuiltinSymbol{
		name: "len",
		impls: []BuiltinImpl{
			builtinLenArray{},
			builtinLenSlice{},
		},
	}
	BuiltinAssert = &BuiltinSymbol{
		name: "assert",
		impls: []BuiltinImpl{
			builtinAssert1{},
			builtinAssert2{},
		},
	}
	BuiltinNew = &BuiltinSymbol{
		name: "new",
		impls: []BuiltinImpl{
			builtinNew{},
		},
	}
	BuiltinMake = &BuiltinSymbol{
		name: "make",
		impls: []BuiltinImpl{
			builtinMakeSlice{},
		},
	}
	BuiltinAppend = &BuiltinSymbol{
		name: "append",
		impls: []BuiltinImpl{
			builtinAppend{},
		},
	}
)

type BuiltinImpl interface {
	Match(args []Expression) bool
	TypeCheck(c *Compiler, pos parser.Position, args []Expression) error
	Type(args []Expression) Type
	Compile(ctx context.Context, c *Compiler, p *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, *Location, error)
}

func BuiltinsSymbols() *SymbolScope {
	s := newScope(nil, "builtins")

	s.put(TypeInt)
	s.put(TypeBool)
	s.put(TypeString)
	s.put(TypeFloat)
	s.put(TypeAny)
	s.put(TypeError)
	s.put(Nil)
	s.put(BuiltinLen)
	s.put(BuiltinAssert)
	s.put(BuiltinExternAssert)
	s.put(BuiltinNew)
	s.put(BuiltinMake)
	s.put(BuiltinAppend)
	s.put(BuiltinExternAppend1)

	return s
}

func BuiltinValues(regs int, symbols *SymbolScope) *ValueScope {
	s := NewValueScope(regs, symbols)

	s.registerType(NilType)

	return s
}

const (
	VarInitFuncName = "__varinit"
)

type BuiltinSymbol struct {
	name    string
	impls   []BuiltinImpl
	shapes  [][]Kind
	externs []*ExternFunction
	parser.Position
}

func (s *BuiltinSymbol) Name() string {
	return s.name
}

func (s *BuiltinSymbol) Type() Type {
	return &BuiltinType{symbol: s}
}

func (s *BuiltinSymbol) CallExpression(c *Compiler, expr *CallExpression) (Expression, error) {
	for _, impl := range s.impls {
		if impl.Match(expr.Args) {
			err := impl.TypeCheck(c, expr.Position, expr.Args)
			if err != nil {
				return expr, err
			}

			return &BuiltinExpression{
				Builtin:  s,
				Impl:     impl,
				Args:     expr.Args,
				Position: expr.Position,
			}, nil
		}
	}

	var argTypeStrs []string
	for _, arg := range expr.Args {
		argTypeStrs = append(argTypeStrs, arg.Type().String())
	}

	return expr, fmt.Errorf("cannot use builtin %s with arguments of type %s", s.name, strings.Join(argTypeStrs, ", "))
}

type BuiltinType struct {
	symbol *BuiltinSymbol
}

func (*BuiltinType) Kind() Kind { return KindBuiltin }

func (t *BuiltinType) String() string {
	return fmt.Sprintf("<builtin %s>", t.symbol.name)
}

func (t *BuiltinType) GlobalName() TypeName {
	return TypeName(fmt.Sprintf("<builtin %s>", t.symbol.name))
}

func (t *BuiltinType) Size() Size {
	return 0
}

func (t *BuiltinType) Name() string {
	return t.symbol.name
}

type BuiltinExpression struct {
	Builtin *BuiltinSymbol
	Impl    BuiltinImpl
	Args    []Expression

	parser.Position
}

func (e *BuiltinExpression) Type() Type {
	return e.Impl.Type(e.Args)
}

type builtinLenArray struct{}

func (b builtinLenArray) Match(args []Expression) bool {
	return len(args) == 1 && args[0].Type().Kind() == KindArray
}

func (b builtinLenArray) TypeCheck(c *Compiler, pos parser.Position, args []Expression) error {
	if len(args) != 1 {
		return pos.WrapError(fmt.Errorf("builtin len expects exactly 1 argument, got %d", len(args)))
	}

	var err error
	args[0], err = c.resolveExpressionTypes(args[0], TypeKind(KindArray))
	if err != nil {
		return err
	}

	if args[0].Type().Kind() != KindArray {
		return pos.WrapError(fmt.Errorf("builtin len expects an array, got %s", args[0].Type()))
	}

	return nil
}

func (b builtinLenArray) Type([]Expression) Type {
	return TypeInt
}

func (b builtinLenArray) Compile(ctx context.Context, c *Compiler, p *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, *Location, error) {
	err := b.TypeCheck(c, pos, args)
	if err != nil {
		return nil, nil, err
	}

	return nil, scope.newImmediate(Int(resolveType(args[0].Type()).(*ArrayType).Length())), nil
}

type builtinLenSlice struct{}

func (b builtinLenSlice) Match(args []Expression) bool {
	return len(args) == 1 && args[0].Type().Kind() == KindSlice
}

func (b builtinLenSlice) TypeCheck(c *Compiler, pos parser.Position, args []Expression) error {
	if len(args) != 1 {
		return pos.WrapError(fmt.Errorf("builtin len expects exactly 1 argument, got %d", len(args)))
	}

	var err error
	args[0], err = c.resolveExpressionTypes(args[0], TypeKind(KindSlice))
	if err != nil {
		return err
	}

	if args[0].Type().Kind() != KindSlice {
		return pos.WrapError(fmt.Errorf("builtin len expects a slice, got %s", args[0].Type()))
	}

	return nil
}

func (b builtinLenSlice) Type([]Expression) Type {
	return TypeInt
}

func (b builtinLenSlice) Compile(ctx context.Context, c *Compiler, p *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, *Location, error) {
	err := b.TypeCheck(c, pos, args)
	if err != nil {
		return nil, nil, err
	}

	var bc BytecodeSnippet

	sliceTmp := scope.allocTemp(args[0].Type())
	defer scope.deallocTemp(sliceTmp)

	sliceBC, sliceDst, err := c.compileBCExpression(ctx, p, args[0], scope, sliceTmp)
	if err != nil {
		return nil, nil, err
	}

	bc.Add(sliceBC...)

	lenLoc, err := sliceDst.AsType(sliceHeader).IndexTuple(1)
	if err != nil {
		return nil, nil, pos.WrapError(fmt.Errorf("failed to index slice header: %w", err))
	}

	return bc, lenLoc, nil
}

type builtinAssert1 struct{}

func (b builtinAssert1) Match(args []Expression) bool {
	return len(args) == 1 && args[0].Type().Kind() == KindBool
}

func (b builtinAssert1) TypeCheck(c *Compiler, pos parser.Position, args []Expression) error {
	if len(args) != 1 {
		return pos.WrapError(fmt.Errorf("builtin assert expects 1 argument, got %d", len(args)))
	}

	var err error
	args[0], err = c.resolveExpressionTypes(args[0], TypeKind(KindBool))
	if err != nil {
		return err
	}

	if args[0].Type().Kind() != KindBool {
		return pos.WrapError(fmt.Errorf("builtin assert expects argument to be a boolean, got %s", args[0].Type()))
	}

	return nil
}

func (b builtinAssert1) Type(args []Expression) Type {
	return TypeVoid
}

func (b builtinAssert1) Compile(ctx context.Context, c *Compiler, prog *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, *Location, error) {
	err := b.TypeCheck(c, pos, args)
	if err != nil {
		return nil, nil, err
	}

	var exprText string
	if rawTexter, ok := args[0].(interface{ RawText() string }); ok {
		exprText = rawTexter.RawText()
	}

	exprTextLiteral := NewLiteral(String(exprText))

	args = append(args, exprTextLiteral)

	return c.compileBCExpression(ctx, prog, &CallExpression{
		Function: &SymbolReferenceExpression{
			name:  BuiltinExternAssert.Name(),
			scope: prog.root,

			Position: pos,
		},
		Args:     args,
		Position: pos,
	}, scope, dst)
}

type builtinAssert2 struct{}

func (b builtinAssert2) Match(args []Expression) bool {
	return len(args) == 2 && args[0].Type().Kind() == KindBool && args[1].Type().Kind() == KindString
}

func (b builtinAssert2) TypeCheck(c *Compiler, pos parser.Position, args []Expression) error {
	if len(args) != 2 {
		return pos.WrapError(fmt.Errorf("builtin assert expects 2 arguments, got %d", len(args)))
	}

	var err error

	args[0], err = c.resolveExpressionTypes(args[0], TypeKind(KindBool))
	if err != nil {
		return err
	}

	if args[0].Type().Kind() != KindBool {
		return pos.WrapError(fmt.Errorf("builtin assert expects first argument to be a boolean, got %s", args[0].Type()))
	}

	args[1], err = c.resolveExpressionTypes(args[1], TypeKind(KindString))
	if err != nil {
		return err
	}

	if args[1].Type().Kind() != KindString {
		return pos.WrapError(fmt.Errorf("builtin assert expects second argument to be a string, got %s", args[1].Type()))
	}

	return nil
}

func (b builtinAssert2) Type(args []Expression) Type {
	return TypeVoid
}

func (b builtinAssert2) Compile(ctx context.Context, c *Compiler, prog *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, *Location, error) {
	err := b.TypeCheck(c, pos, args)
	if err != nil {
		return nil, nil, err
	}

	return c.compileBCExpression(ctx, prog, &CallExpression{
		Function: &SymbolReferenceExpression{
			name:  BuiltinExternAssert.Name(),
			scope: prog.root,

			Position: pos,
		},
		Args:     args,
		Position: pos,
	}, scope, dst)
}

type builtinNew struct{}

func (b builtinNew) Match(args []Expression) bool {
	return len(args) == 1 && args[0].Type().Kind() == KindType
}

func (b builtinNew) TypeCheck(c *Compiler, pos parser.Position, args []Expression) error {
	if len(args) != 1 {
		return pos.WrapError(fmt.Errorf("builtin new expects 1 argument, got %d", len(args)))
	}

	if args[0].Type().Kind() != KindType {
		return pos.WrapError(fmt.Errorf("builtin new expects a type, got value expression %q", args[0]))
	}

	return nil
}

func (b builtinNew) Type(args []Expression) Type {
	return NewPointerType(dereferenceType(args[0].Type()).(*TypeType).Type)
}

func (b builtinNew) Compile(ctx context.Context, c *Compiler, prog *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, *Location, error) {
	err := b.TypeCheck(c, pos, args)
	if err != nil {
		return nil, nil, err
	}

	var bc BytecodeSnippet
	bc.Alloc(dst, scope.newImmediate(Int(resolveType(args[0].Type()).Size())))

	return bc, dst, nil
}

type builtinMakeSlice struct{}

func (b builtinMakeSlice) Match(args []Expression) bool {
	switch len(args) {
	case 2:
		return args[0].Type().Kind() == KindType && args[1].Type().Kind() == KindInt
	case 3:
		return args[0].Type().Kind() == KindType && args[1].Type().Kind() == KindInt && args[2].Type().Kind() == KindInt
	default:
		return false
	}
}

func (b builtinMakeSlice) TypeCheck(c *Compiler, pos parser.Position, args []Expression) error {
	if len(args) != 2 && len(args) != 3 {
		return pos.WrapError(fmt.Errorf("builtin make expects 2 or 3 arguments, got %d", len(args)))
	}
	var err error

	args[0], err = c.resolveExpressionTypes(args[0], TypeKind(KindType))
	if err != nil {
		return err
	}
	if args[0].Type().Kind() != KindType {
		return pos.WrapError(fmt.Errorf("builtin make expects a type, got value expression %q", args[0]))
	}

	args[1], err = c.resolveExpressionTypes(args[1], TypeKind(KindInt))
	if err != nil {
		return err
	}

	if args[1].Type().Kind() != KindInt {
		return pos.WrapError(fmt.Errorf("builtin make expects length argument to be an int, got %s", args[1].Type()))
	}

	if len(args) == 3 {
		args[2], err = c.resolveExpressionTypes(args[2], TypeKind(KindInt))
		if err != nil {
			return err
		}

		if args[2].Type().Kind() != KindInt {
			return pos.WrapError(fmt.Errorf("builtin make expects capacity argument to be int, got %s", args[2].Type()))
		}
	}

	return nil
}

func (b builtinMakeSlice) Type(args []Expression) Type {
	return dereferenceType(args[0].Type()).(*TypeType).Type
}

func (b builtinMakeSlice) Compile(ctx context.Context, c *Compiler, prog *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, *Location, error) {
	err := b.TypeCheck(c, pos, args)
	if err != nil {
		return nil, nil, err
	}

	var bc BytecodeSnippet

	hdrDst := dst.AsType(sliceHeader)
	hdrLenDst, err := hdrDst.IndexTuple(1)
	if err != nil {
		return nil, nil, pos.WrapError(fmt.Errorf("failed to index slice header: %w", err))
	}

	hdrLenBC, hdrLenLoc, err := c.compileBCExpression(ctx, prog, args[1], scope, hdrLenDst)
	if err != nil {
		return nil, nil, err
	}

	bc.Add(hdrLenBC...)

	if !hdrLenLoc.SameMemory(hdrLenDst) {
		bc.Mov(hdrLenDst, hdrLenLoc)
	}

	hdrCapDst, err := hdrDst.IndexTuple(2)
	if err != nil {
		return nil, nil, pos.WrapError(fmt.Errorf("failed to index slice header: %w", err))
	}

	var hdrCapLoc *Location

	if len(args) == 3 {
		var hdrCapBC BytecodeSnippet
		hdrCapBC, hdrCapLoc, err = c.compileBCExpression(ctx, prog, args[2], scope, hdrCapDst)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(hdrCapBC...)

		if !hdrCapLoc.SameMemory(hdrCapDst) {
			bc.Mov(hdrCapDst, hdrCapLoc)
		}
	} else {
		hdrCapLoc = hdrLenLoc
		bc.Mov(hdrCapDst, hdrLenLoc)
	}

	hdrPtrDst, err := hdrDst.IndexTuple(0)
	if err != nil {
		return nil, nil, pos.WrapError(fmt.Errorf("failed to index slice header: %w", err))
	}

	elemTyp := dereferenceType(args[0].Type()).(*TypeType).Type.(*SliceType).Elem()
	bc.Alloc(hdrPtrDst, hdrCapLoc.Mul(scope.newImmediate(Int(elemTyp.Size()))))

	return bc, dst, nil
}

type builtinAppend struct{}

func (b builtinAppend) Match(args []Expression) bool {
	// TODO: variadic
	return len(args) == 2 && args[0].Type().Kind() == KindSlice
}

func (b builtinAppend) TypeCheck(c *Compiler, pos parser.Position, args []Expression) error {
	if len(args) != 2 {
		return pos.WrapError(fmt.Errorf("builtin append expects 2 arguments, got %d", len(args)))
	}
	var err error
	args[0], err = c.resolveExpressionTypes(args[0], TypeKind(KindSlice))
	if err != nil {
		return err
	}
	if args[0].Type().Kind() != KindSlice {
		return pos.WrapError(fmt.Errorf("builtin append expects a slice, got %s", args[0].Type()))
	}

	elemTyp := resolveType(args[0].Type()).(*SliceType).Elem()

	args[1], err = c.resolveExpressionTypes(args[1], elemTyp)
	if err != nil {
		return err
	}

	if !IsAssignableTo(args[1].Type(), elemTyp) {
		return pos.WrapError(fmt.Errorf("builtin append expects element to be assignable to slice element type %s, got %s", elemTyp, args[1].Type()))
	}

	return nil
}

func (b builtinAppend) Type(args []Expression) Type {
	if len(args) == 0 {
		return UnknownType
	}
	return args[0].Type()
}

func (b builtinAppend) Compile(ctx context.Context, c *Compiler, prog *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, *Location, error) {
	err := b.TypeCheck(c, pos, args)
	if err != nil {
		return nil, nil, err
	}

	var bc BytecodeSnippet
	sliceTmp := scope.allocTemp(args[0].Type())
	defer scope.deallocTemp(sliceTmp)
	sliceBC, sliceDst, err := c.compileBCExpression(ctx, prog, args[0], scope, sliceTmp)
	if err != nil {
		return nil, nil, err
	}
	bc.Add(sliceBC...)

	elemTmp := scope.allocTemp(resolveType(args[0].Type()).(*SliceType).Elem())
	defer scope.deallocTemp(elemTmp)
	elemBC, elemLoc, err := c.compileBCExpression(ctx, prog, args[1], scope, elemTmp)
	if err != nil {
		return nil, nil, err
	}

	bc.Add(elemBC...)

	sliceHdrDst := sliceDst.AsType(sliceHeader)

	callScope := scope.sub(scope.symbols)

	dataLoc, err := sliceHdrDst.IndexTuple(0)
	if err != nil {
		return nil, nil, pos.WrapError(fmt.Errorf("failed to index slice header: %w", err))
	}

	lenLoc, err := sliceHdrDst.IndexTuple(1)
	if err != nil {
		return nil, nil, pos.WrapError(fmt.Errorf("failed to index slice header: %w", err))
	}

	capLoc, err := sliceHdrDst.IndexTuple(2)
	if err != nil {
		return nil, nil, pos.WrapError(fmt.Errorf("failed to index slice header: %w", err))
	}

	elemTyp := resolveType(args[0].Type()).(*SliceType).Elem()
	if elemTyp.Size() > 1 {
		elemLoc = elemLoc.AddressOf()
	}
	sizeLoc := scope.newImmediate(Int(elemTyp.Size()))

	retPtrLoc := dst.AddressOf()

	dataArg := callScope.newArg("data", 0, TypeKind(KindPointer))
	bc.Mov(dataArg, dataLoc)

	lenArg := callScope.newArg("len", 1, TypeKind(KindInt))
	bc.Mov(lenArg, lenLoc)

	capArg := callScope.newArg("cap", 2, TypeKind(KindInt))
	bc.Mov(capArg, capLoc)

	elemArg := callScope.newArg("elem", 3, TypeKind(KindPointer))
	bc.Mov(elemArg, elemLoc)

	sizeArg := callScope.newArg("size", 4, TypeKind(KindInt))
	bc.Mov(sizeArg, sizeLoc)

	retPtrArg := callScope.newArg("retPtr", 5, TypeKind(KindInt))
	bc.Mov(retPtrArg, retPtrLoc)

	bc.Mov(callScope.SP(), callScope.SP().Add(callScope.newImmediate(Int(6))))

	callLoc, ok := scope.Get(BuiltinExternAppend1.Name())
	if !ok {
		return nil, nil, pos.WrapError(fmt.Errorf("failed to find builtin append function %q", BuiltinExternAppend1.Name()))
	}

	bc.Add(Cal{Func: callLoc.Operand})

	return bc, dst, nil
}
