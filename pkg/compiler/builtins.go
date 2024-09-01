package compiler

import (
	"context"
	"fmt"
	"strings"

	"github.com/rhino1998/aeon/pkg/compiler/air"
	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/types"
	"github.com/rhino1998/aeon/pkg/parser"
)

var (
	TypeInt             = types.NewDerived("int", nil, types.Kind(kinds.Int))
	TypeString          = types.NewDerived("string", nil, types.Kind(kinds.String))
	TypeBool            = types.NewDerived("bool", nil, types.Kind(kinds.Bool))
	TypeFloat           = types.NewDerived("float", nil, types.Kind(kinds.Float))
	TypeAny             = types.NewDerived("any", nil, types.NewInterface())
	TypeNil             = types.NewDerived("nil", nil, types.Nil)
	TypeError           = types.NewDerived("error", nil, TypeErrorInterface)
	TypeErrorInterface  = types.NewInterface().With("Error", nil, TypeString)
	BuiltinExternAssert = &ExternFunction{
		name: "__builtin_assert",
		parameters: []*Variable{
			&Variable{
				typ: types.Kind(kinds.Bool),
			},
			&Variable{
				typ: types.Kind(kinds.String),
			},
		},
		ret: types.Void,
	}
	ExternPrint = &ExternFunction{
		name: "print",
		parameters: []*Variable{
			&Variable{
				typ:      types.NewSlice(TypeAny),
				variadic: true,
			},
		},
		ret: types.Void,
	}
	ExternPrintf = &ExternFunction{
		name: "printf",
		parameters: []*Variable{
			&Variable{
				typ: TypeString,
			},
			&Variable{
				typ:      types.NewSlice(TypeAny),
				variadic: true,
			},
		},
		ret: types.Void,
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
	Type(args []Expression) types.Type
	Compile(ctx context.Context, c *Compiler, p *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *air.Value) (air.Snippet, *air.Value, error)
}

func BuiltinsSymbols() *SymbolScope {
	s := newScope(nil, "builtins")

	s.put(TypeInt)
	s.put(TypeBool)
	s.put(TypeString)
	s.put(TypeFloat)
	s.put(TypeAny)
	s.put(TypeError)
	s.put(TypeNil)
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

	s.registerType(TypeNil)

	return s
}

const (
	VarInitFuncName = "__varinit"
)

type BuiltinSymbol struct {
	name    string
	impls   []BuiltinImpl
	externs []*ExternFunction
	parser.Position
}

func (s *BuiltinSymbol) Name() string {
	return s.name
}

func (s *BuiltinSymbol) Type() types.Type {
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

func (*BuiltinType) Kind() kinds.Kind { return kinds.Builtin }

func (t *BuiltinType) String() string {
	return fmt.Sprintf("<builtin %s>", t.symbol.name)
}

func (t *BuiltinType) GlobalName() types.Name {
	return types.Name(fmt.Sprintf("<builtin %s>", t.symbol.name))
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

func (e *BuiltinExpression) Type() types.Type {
	return e.Impl.Type(e.Args)
}

type builtinLenArray struct{}

func (b builtinLenArray) Match(args []Expression) bool {
	return len(args) == 1 && args[0].Type().Kind() == kinds.Array
}

func (b builtinLenArray) TypeCheck(c *Compiler, pos parser.Position, args []Expression) error {
	if len(args) != 1 {
		return pos.WrapError(fmt.Errorf("builtin len expects exactly 1 argument, got %d", len(args)))
	}

	var err error
	args[0], err = c.resolveExpressionTypes(args[0], types.Kind(kinds.Array))
	if err != nil {
		return err
	}

	if args[0].Type().Kind() != kinds.Array {
		return pos.WrapError(fmt.Errorf("builtin len expects an array, got %s", args[0].Type()))
	}

	return nil
}

func (b builtinLenArray) Type([]Expression) types.Type {
	return TypeInt
}

func (b builtinLenArray) Compile(ctx context.Context, c *Compiler, p *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *air.Value) (air.Snippet, *air.Value, error) {
	err := b.TypeCheck(c, pos, args)
	if err != nil {
		return nil, nil, err
	}

	return nil, scope.newImmediate(air.Int(types.Resolve(args[0].Type()).(*types.Array).Length())), nil
}

type builtinLenSlice struct{}

func (b builtinLenSlice) Match(args []Expression) bool {
	return len(args) == 1 && args[0].Type().Kind() == kinds.Slice
}

func (b builtinLenSlice) TypeCheck(c *Compiler, pos parser.Position, args []Expression) error {
	if len(args) != 1 {
		return pos.WrapError(fmt.Errorf("builtin len expects exactly 1 argument, got %d", len(args)))
	}

	var err error
	args[0], err = c.resolveExpressionTypes(args[0], types.Kind(kinds.Slice))
	if err != nil {
		return err
	}

	if args[0].Type().Kind() != kinds.Slice {
		return pos.WrapError(fmt.Errorf("builtin len expects a slice, got %s", args[0].Type()))
	}

	return nil
}

func (b builtinLenSlice) Type([]Expression) types.Type {
	return TypeInt
}

func (b builtinLenSlice) Compile(ctx context.Context, c *Compiler, p *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *air.Value) (air.Snippet, *air.Value, error) {
	err := b.TypeCheck(c, pos, args)
	if err != nil {
		return nil, nil, err
	}

	var bc air.Snippet

	sliceTmp := scope.allocTemp(args[0].Type())

	sliceBC, sliceDst, err := c.compileBCExpression(ctx, p, args[0], scope, sliceTmp)
	if err != nil {
		return nil, nil, err
	}

	bc.Add(sliceBC...)

	lenLoc, err := sliceDst.AsType(air.SliceTuple).IndexTuple(1)
	if err != nil {
		return nil, nil, pos.WrapError(fmt.Errorf("failed to index slice header: %w", err))
	}

	return bc, lenLoc, nil
}

type builtinCapSlice struct{}

func (b builtinCapSlice) Match(args []Expression) bool {
	return cap(args) == 1 && args[0].Type().Kind() == kinds.Slice
}

func (b builtinCapSlice) TypeCheck(c *Compiler, pos parser.Position, args []Expression) error {
	if cap(args) != 1 {
		return pos.WrapError(fmt.Errorf("builtin cap expects exactly 1 argument, got %d", cap(args)))
	}

	var err error
	args[0], err = c.resolveExpressionTypes(args[0], types.Kind(kinds.Slice))
	if err != nil {
		return err
	}

	if args[0].Type().Kind() != kinds.Slice {
		return pos.WrapError(fmt.Errorf("builtin cap expects a slice, got %s", args[0].Type()))
	}

	return nil
}

func (b builtinCapSlice) Type([]Expression) types.Type {
	return types.Kind(kinds.Int)
}

func (b builtinCapSlice) Compile(ctx context.Context, c *Compiler, p *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *air.Value) (air.Snippet, *air.Value, error) {
	err := b.TypeCheck(c, pos, args)
	if err != nil {
		return nil, nil, err
	}

	var bc air.Snippet

	sliceTmp := scope.allocTemp(args[0].Type())

	sliceBC, sliceDst, err := c.compileBCExpression(ctx, p, args[0], scope, sliceTmp)
	if err != nil {
		return nil, nil, err
	}

	bc.Add(sliceBC...)

	capLoc, err := sliceDst.AsType(air.SliceTuple).IndexTuple(2)
	if err != nil {
		return nil, nil, pos.WrapError(fmt.Errorf("failed to index slice header: %w", err))
	}

	return bc, capLoc, nil
}

type builtinAssert1 struct{}

func (b builtinAssert1) Match(args []Expression) bool {
	return len(args) == 1 && args[0].Type().Kind() == kinds.Bool
}

func (b builtinAssert1) TypeCheck(c *Compiler, pos parser.Position, args []Expression) error {
	if len(args) != 1 {
		return pos.WrapError(fmt.Errorf("builtin assert expects 1 argument, got %d", len(args)))
	}

	var err error
	args[0], err = c.resolveExpressionTypes(args[0], types.Kind(kinds.Bool))
	if err != nil {
		return err
	}

	if args[0].Type().Kind() != kinds.Bool {
		return pos.WrapError(fmt.Errorf("builtin assert expects argument to be a boolean, got %s", args[0].Type()))
	}

	return nil
}

func (b builtinAssert1) Type(args []Expression) types.Type {
	return types.Void
}

func (b builtinAssert1) Compile(ctx context.Context, c *Compiler, prog *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *air.Value) (air.Snippet, *air.Value, error) {
	err := b.TypeCheck(c, pos, args)
	if err != nil {
		return nil, nil, err
	}

	var exprText string
	if rawTexter, ok := args[0].(interface{ RawText() string }); ok {
		exprText = rawTexter.RawText()
	}

	exprTextLiteral := NewLiteral(air.String(exprText))

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
	return len(args) == 2 && args[0].Type().Kind() == kinds.Bool && args[1].Type().Kind() == kinds.String
}

func (b builtinAssert2) TypeCheck(c *Compiler, pos parser.Position, args []Expression) error {
	if len(args) != 2 {
		return pos.WrapError(fmt.Errorf("builtin assert expects 2 arguments, got %d", len(args)))
	}

	var err error

	args[0], err = c.resolveExpressionTypes(args[0], types.Kind(kinds.Bool))
	if err != nil {
		return err
	}

	if args[0].Type().Kind() != kinds.Bool {
		return pos.WrapError(fmt.Errorf("builtin assert expects first argument to be a boolean, got %s", args[0].Type()))
	}

	args[1], err = c.resolveExpressionTypes(args[1], types.Kind(kinds.String))
	if err != nil {
		return err
	}

	if args[1].Type().Kind() != kinds.String {
		return pos.WrapError(fmt.Errorf("builtin assert expects second argument to be a string, got %s", args[1].Type()))
	}

	return nil
}

func (b builtinAssert2) Type(args []Expression) types.Type {
	return types.Void
}

func (b builtinAssert2) Compile(ctx context.Context, c *Compiler, prog *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *air.Value) (air.Snippet, *air.Value, error) {
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
	return len(args) == 1 && args[0].Type().Kind() == kinds.Type
}

func (b builtinNew) TypeCheck(c *Compiler, pos parser.Position, args []Expression) error {
	if len(args) != 1 {
		return pos.WrapError(fmt.Errorf("builtin new expects 1 argument, got %d", len(args)))
	}

	if args[0].Type().Kind() != kinds.Type {
		return pos.WrapError(fmt.Errorf("builtin new expects a type, got value expression %q", args[0]))
	}

	return nil
}

func (b builtinNew) Type(args []Expression) types.Type {
	return types.NewPointer(types.Dereference(args[0].Type()).(*types.TypeType).Type)
}

func (b builtinNew) Compile(ctx context.Context, c *Compiler, prog *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *air.Value) (air.Snippet, *air.Value, error) {
	err := b.TypeCheck(c, pos, args)
	if err != nil {
		return nil, nil, err
	}

	size, err := air.TypeSize(types.Dereference(args[0].Type()).(*types.TypeType).Type)
	if err != nil {
		return nil, nil, pos.WrapError(fmt.Errorf("failed to get size of type %s: %w", args[0].Type(), err))
	}

	var bc air.Snippet
	bc.Alloc(dst.Operand, air.IntOperand(size))

	return bc, dst, nil
}

type builtinMakeSlice struct{}

func (b builtinMakeSlice) Match(args []Expression) bool {
	switch len(args) {
	case 2:
		return args[0].Type().Kind() == kinds.Type && args[1].Type().Kind() == kinds.Int
	case 3:
		return args[0].Type().Kind() == kinds.Type && args[1].Type().Kind() == kinds.Int && args[2].Type().Kind() == kinds.Int
	default:
		return false
	}
}

func (b builtinMakeSlice) TypeCheck(c *Compiler, pos parser.Position, args []Expression) error {
	if len(args) != 2 && len(args) != 3 {
		return pos.WrapError(fmt.Errorf("builtin make expects 2 or 3 arguments, got %d", len(args)))
	}
	var err error

	args[0], err = c.resolveExpressionTypes(args[0], types.Kind(kinds.Type))
	if err != nil {
		return err
	}
	if args[0].Type().Kind() != kinds.Type {
		return pos.WrapError(fmt.Errorf("builtin make expects a type, got value expression %q", args[0]))
	}

	args[1], err = c.resolveExpressionTypes(args[1], types.Kind(kinds.Int))
	if err != nil {
		return err
	}

	if args[1].Type().Kind() != kinds.Int {
		return pos.WrapError(fmt.Errorf("builtin make expects length argument to be an int, got %s", args[1].Type()))
	}

	if len(args) == 3 {
		args[2], err = c.resolveExpressionTypes(args[2], types.Kind(kinds.Int))
		if err != nil {
			return err
		}

		if args[2].Type().Kind() != kinds.Int {
			return pos.WrapError(fmt.Errorf("builtin make expects capacity argument to be int, got %s", args[2].Type()))
		}
	}

	return nil
}

func (b builtinMakeSlice) Type(args []Expression) types.Type {
	return types.Dereference(args[0].Type()).(*types.TypeType).Type
}

func (b builtinMakeSlice) Compile(ctx context.Context, c *Compiler, prog *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *air.Value) (air.Snippet, *air.Value, error) {
	err := b.TypeCheck(c, pos, args)
	if err != nil {
		return nil, nil, err
	}

	var bc air.Snippet

	lenDst, err := dst.SliceLen()
	if err != nil {
		return nil, nil, pos.WrapError(fmt.Errorf("failed to index slice header: %w", err))
	}

	lenBC, lenLoc, err := c.compileBCExpression(ctx, prog, args[1], scope, lenDst)
	if err != nil {
		return nil, nil, err
	}

	bc.Add(lenBC...)

	err = bc.Mov(lenDst, lenLoc)
	if err != nil {
		return nil, nil, pos.WrapError(fmt.Errorf("failed to move length to slice header: %w", err))
	}

	capDst, err := dst.SliceCap()
	if err != nil {
		return nil, nil, pos.WrapError(fmt.Errorf("failed to index slice header: %w", err))
	}

	var capLoc *air.Value

	if len(args) == 3 {
		var capBC air.Snippet
		capBC, capLoc, err = c.compileBCExpression(ctx, prog, args[2], scope, capDst)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(capBC...)

		bc.Mov(capDst, capLoc)
	} else {
		capLoc = lenLoc
		bc.Mov(capDst, lenLoc)
	}

	hdrPtrDst, err := dst.AsType(air.SliceTuple).IndexTuple(0)
	if err != nil {
		return nil, nil, pos.WrapError(fmt.Errorf("failed to index slice header: %w", err))
	}

	elemTyp := types.Dereference(args[0].Type()).(*types.TypeType).Type.(*types.Slice).Elem()
	elemSize, err := air.TypeSize(elemTyp)
	if err != nil {
		return nil, nil, pos.WrapError(fmt.Errorf("failed to get size of slice element type: %w", err))
	}
	bc.Alloc(hdrPtrDst.Operand, capLoc.Mul(scope.newImmediate(air.Int(elemSize))).Operand)

	return bc, dst, nil
}

type builtinAppend struct{}

func (b builtinAppend) Match(args []Expression) bool {
	// TODO: variadic
	return len(args) == 2 && args[0].Type().Kind() == kinds.Slice
}

func (b builtinAppend) TypeCheck(c *Compiler, pos parser.Position, args []Expression) error {
	if len(args) != 2 {
		return pos.WrapError(fmt.Errorf("builtin append expects 2 arguments, got %d", len(args)))
	}
	var err error
	args[0], err = c.resolveExpressionTypes(args[0], types.Kind(kinds.Slice))
	if err != nil {
		return err
	}
	if args[0].Type().Kind() != kinds.Slice {
		return pos.WrapError(fmt.Errorf("builtin append expects a slice, got %s", args[0].Type()))
	}

	elemTyp := types.Resolve(args[0].Type()).(*types.Slice).Elem()

	args[1], err = c.resolveExpressionTypes(args[1], elemTyp)
	if err != nil {
		return err
	}

	if !types.IsAssignableTo(args[1].Type(), elemTyp) {
		return pos.WrapError(fmt.Errorf("builtin append expects element to be assignable to slice element type %s, got %s", elemTyp, args[1].Type()))
	}

	return nil
}

func (b builtinAppend) Type(args []Expression) types.Type {
	if len(args) == 0 {
		return types.Unknown
	}
	return args[0].Type()
}

func (b builtinAppend) Compile(ctx context.Context, c *Compiler, prog *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *air.Value) (air.Snippet, *air.Value, error) {
	err := b.TypeCheck(c, pos, args)
	if err != nil {
		return nil, nil, err
	}

	var bc air.Snippet
	sliceTmp := scope.allocTemp(args[0].Type())
	sliceBC, sliceLoc, err := c.compileBCExpression(ctx, prog, args[0], scope, sliceTmp)
	if err != nil {
		return nil, nil, err
	}
	bc.Add(sliceBC...)

	elemTmp := scope.allocTemp(args[1].Type())
	elemBC, elemLoc, err := c.compileBCExpression(ctx, prog, args[1], scope, elemTmp)
	if err != nil {
		return nil, nil, err
	}

	bc.Add(elemBC...)

	err = bc.App(dst, sliceLoc, elemLoc)
	if err != nil {
		return nil, nil, pos.WrapError(err)
	}

	return bc, dst, nil
}
