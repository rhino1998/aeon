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
				return nil, err
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

type builtinAssert1 struct{}

func (b builtinAssert1) Match(args []Expression) bool {
	return len(args) == 1 && args[0].Type().Kind() == KindBool
}

func (b builtinAssert1) TypeCheck(c *Compiler, pos parser.Position, args []Expression) error {
	if len(args) != 1 {
		return pos.WrapError(fmt.Errorf("builtin assert expects 1 argument, got %d", len(args)))
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

	if args[0].Type().Kind() != KindBool {
		return pos.WrapError(fmt.Errorf("builtin assert expects first argument to be a boolean, got %s", args[0].Type()))
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
	bc.Alloc(dst)

	return bc, dst, nil
}
