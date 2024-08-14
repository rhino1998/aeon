package compiler

var (
	IntType = &DerivedType{
		name:       "int",
		underlying: TypeKind(KindInt),
	}

	BoolType = &DerivedType{
		name:       "bool",
		underlying: TypeKind(KindBool),
	}

	FloatType = &DerivedType{
		name:       "float",
		underlying: TypeKind(KindFloat),
	}

	StringType = &DerivedType{
		name:       "string",
		underlying: TypeKind(KindString),
	}

	AnyType = &DerivedType{
		name:       "any",
		underlying: &InterfaceType{},
	}
)

var (
	NilConstant = &Constant{
		name:               "nil",
		typ:                NilType,
		ConstantExpression: NewLiteral(NilValue),
	}
)

func BuiltinsSymbols() *SymbolScope {
	s := newScope(nil, "builtins")

	s.put(IntType)
	s.put(BoolType)
	s.put(StringType)
	s.put(FloatType)
	s.put(AnyType)
	s.put(NilConstant)

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
