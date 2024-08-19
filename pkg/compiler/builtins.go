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
	TypeAny             = NewDerivedType("any", nil, &InterfaceType{})
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
		impls: []builtinImpl{
			{
				shape:   []Kind{KindArray},
				compile: builtinImplArrayLen(),
				ret:     TypeInt},
			{
				shape:   []Kind{KindSlice},
				compile: builtinImplUnimp(),
				ret:     TypeInt},
			{
				shape:   []Kind{KindMap},
				compile: builtinImplUnimp(),
				ret:     TypeInt,
			},
		},
	}
	BuiltinAssert = &BuiltinSymbol{
		name: "assert",
		impls: []builtinImpl{
			{
				shape:   []Kind{KindBool},
				compile: builtinImplAssert1,
				ret:     TypeVoid,
			},
			{
				shape:   []Kind{KindBool, KindString},
				compile: builtinImplExtern(BuiltinExternAssert),
				ret:     TypeVoid,
			},
		},
	}
)

type builtinImpl struct {
	shape   []Kind
	compile builtinImplFunc
	ret     Type
}

type builtinImplFunc func(ctx context.Context, prog *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, *Location, error)

func builtinImplExtern(extern *ExternFunction) builtinImplFunc {
	return func(ctx context.Context, prog *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, *Location, error) {
		return prog.compileBCExpression(ctx, &CallExpression{
			Function: &SymbolReferenceExpression{
				name:  extern.name,
				scope: prog.root,

				Position: pos,
			},
			Args:     args,
			Position: pos,
		}, scope, dst)
	}
}

func builtinImplArrayLen() builtinImplFunc {
	return func(ctx context.Context, prog *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, *Location, error) {
		if len(args) != 1 {
			return nil, nil, pos.WrapError(fmt.Errorf("builtin len expects exactly 1 argument, got %d", len(args)))
		}

		switch arg := resolveType(args[0].Type()).(type) {
		case *ArrayType:
			return nil, scope.newImmediate(Int(arg.Length())), nil
		default:
			return nil, nil, pos.WrapError(fmt.Errorf("cannot get length of %s", args[0].Type()))
		}
	}
}

func builtinImplAssert1(ctx context.Context, prog *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, *Location, error) {
	var exprText string
	if rawTexter, ok := args[0].(interface{ RawText() string }); ok {
		exprText = rawTexter.RawText()
	}

	exprTextLiteral := NewLiteral(String(exprText))

	args = append(args, exprTextLiteral)

	return builtinImplExtern(BuiltinExternAssert)(ctx, prog, pos, args, scope, dst)
}

func builtinImplUnimp() builtinImplFunc {
	return func(ctx context.Context, prog *Program, pos parser.Position, args []Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, *Location, error) {
		return nil, nil, pos.WrapError(fmt.Errorf("builtin currently unimplemented"))
	}
}

func BuiltinsSymbols() *SymbolScope {
	s := newScope(nil, "builtins")

	s.put(TypeInt)
	s.put(TypeBool)
	s.put(TypeString)
	s.put(TypeFloat)
	s.put(TypeAny)
	s.put(Nil)
	s.put(BuiltinLen)
	s.put(BuiltinAssert)
	s.put(BuiltinExternAssert)

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
	impls   []builtinImpl
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

func (s *BuiltinSymbol) impl(args []Expression) (builtinImpl, error) {
	for _, impl := range s.impls {
		if len(impl.shape) != len(args) {
			continue
		}

		for i := range len(impl.shape) {
			if args[i].Type().Kind() == impl.shape[i] {
				continue
			}
		}

		return impl, nil
	}

	var argTypeStrs []string
	for _, arg := range args {
		argTypeStrs = append(argTypeStrs, arg.Type().String())
	}

	return builtinImpl{}, fmt.Errorf("cannot use builtin %s with arguments of type %s", s.name, strings.Join(argTypeStrs, ", "))
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
	Args    []Expression

	parser.Position
}

func (e *BuiltinExpression) Type() Type {
	impl, err := e.Builtin.impl(e.Args)
	if err != nil {
		return UnknownType
	}

	return impl.ret
}
