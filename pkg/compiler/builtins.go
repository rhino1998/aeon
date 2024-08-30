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
	TypeError           = NewDerivedType("error", nil, TypeErrorInterface)
	TypeErrorInterface  = NewInterfaceType().With("Error", nil, TypeString)
	BuiltinExternAssert = &ExternFunction{
		name: "__builtin_assert",
		parameters: []*Variable{
			&Variable{
				typ: TypeKind(KindBool),
			},
			&Variable{
				typ: TypeKind(KindString),
			},
		},
		ret: TypeVoid,
	}
	ExternPrint = &ExternFunction{
		name: "print",
		parameters: []*Variable{
			&Variable{
				typ:      &SliceType{elem: TypeAny},
				variadic: true,
			},
		},
		ret: TypeVoid,
	}
	ExternPrintf = &ExternFunction{
		name: "printf",
		parameters: []*Variable{
			&Variable{
				typ: TypeString,
			},
			&Variable{
				typ:      &SliceType{elem: TypeAny},
				variadic: true,
			},
		},
		ret: TypeVoid,
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
	BuiltinCap = &BuiltinSymbol{
		name: "cap",
		impls: []BuiltinImpl{
			builtinCapSlice{},
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
	s.put(Nil{})
	s.put(BuiltinLen)
	s.put(BuiltinCap)
	s.put(BuiltinAssert)
	s.put(BuiltinExternAssert)
	s.put(BuiltinNew)
	s.put(BuiltinMake)
	s.put(BuiltinAppend)
	s.put(ExternPrint)
	s.put(ExternPrintf)

	return s
}

func BuiltinValues(regs int, symbols *SymbolScope) *ValueScope {
	s := NewValueScope(regs, symbols)

	s.registerType(Nil{})

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

	return expr, expr.WrapError(fmt.Errorf("cannot use builtin %s with arguments of type %s", s.name, strings.Join(argTypeStrs, ", ")))
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

	return nil, scope.newImmediate(Int(*ResolveType(args[0].Type()).(*ArrayType).Length())), nil
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

type builtinCapSlice struct{}

func (b builtinCapSlice) Match(args []Expression) bool {
	return cap(args) == 1 && args[0].Type().Kind() == KindSlice
}

func (b builtinCapSlice) TypeCheck(c *Compiler, pos parser.Position, args []Expression) error {
	if cap(args) != 1 {
		return pos.WrapError(fmt.Errorf("builtin cap expects exactly 1 argument, got %d", cap(args)))
	}

	var err error
	args[0], err = c.resolveExpressionTypes(args[0], TypeKind(KindSlice))
	if err != nil {
		return err
	}

	if args[0].Type().Kind() != KindSlice {
		return pos.WrapError(fmt.Errorf("builtin cap expects a slice, got %s", args[0].Type()))
	}

	return nil
}

func (b builtinCapSlice) Type([]Expression) Type {
	return TypeInt
}

func (b builtinCapSlice) Compile(ctx context.Context, c *Compiler, p *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, *Location, error) {
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

	capLoc, err := sliceDst.AsType(sliceHeader).IndexTuple(2)
	if err != nil {
		return nil, nil, pos.WrapError(fmt.Errorf("failed to index slice header: %w", err))
	}

	return bc, capLoc, nil
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
	return NewPointerType(DereferenceType(args[0].Type()).(*TypeType).Type)
}

func (b builtinNew) Compile(ctx context.Context, c *Compiler, prog *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, *Location, error) {
	err := b.TypeCheck(c, pos, args)
	if err != nil {
		return nil, nil, err
	}

	var bc BytecodeSnippet
	bc.Alloc(dst, scope.newImmediate(Int(ResolveType(args[0].Type()).Size())))

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
	return DereferenceType(args[0].Type()).(*TypeType).Type
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

	elemTyp := DereferenceType(args[0].Type()).(*TypeType).Type.(*SliceType).Elem()
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

	elemTyp := ResolveType(args[0].Type()).(*SliceType).Elem()

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
	sliceBC, sliceLoc, err := c.compileBCExpression(ctx, prog, args[0], scope, sliceTmp)
	if err != nil {
		return nil, nil, err
	}
	bc.Add(sliceBC...)

	elemTmp := scope.allocTemp(args[1].Type())
	defer scope.deallocTemp(elemTmp)
	elemBC, elemLoc, err := c.compileBCExpression(ctx, prog, args[1], scope, elemTmp)
	if err != nil {
		return nil, nil, err
	}

	bc.Add(elemBC...)

	bc.App(dst, sliceLoc, elemLoc, args[1].Type().Size())

	return bc, dst, nil
}
