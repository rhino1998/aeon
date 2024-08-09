package compiler

var (
	IntType = &BasicType{
		kind: KindInt,
		name: "int",
		size: 1,
	}

	BoolType = &BasicType{
		kind: KindBool,
		name: "bool",
		size: 1,
	}

	StringType = &BasicType{
		kind: KindString,
		name: "string",
		size: 1,
	}

	FloatType = &BasicType{
		kind: KindFloat,
		name: "float",
		size: 1,
	}

	AnyType = &InterfaceType{}
)

func BuiltinsSymbols() *SymbolScope {
	s := newScope(nil, "builtins")

	s.put(IntType)
	s.put(BoolType)
	s.put(StringType)
	s.put(FloatType)

	return s
}

func BuiltinValues(regs int, symbols *SymbolScope) *ValueScope {
	s := NewValueScope(regs, symbols)

	return s
}
