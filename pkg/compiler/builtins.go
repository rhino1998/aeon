package compiler

var (
	IntType = &BasicType{
		kind: KindInt,
		name: "int",
	}

	BoolType = &BasicType{
		kind: KindBool,
		name: "bool",
	}

	StringType = &BasicType{
		kind: KindString,
		name: "string",
	}

	FloatType = &BasicType{
		kind: KindFloat,
		name: "float",
	}
)

func builtins() *SymbolScope {
	s := newScope(nil, "builtins")

	s.put(IntType)
	s.put(BoolType)
	s.put(StringType)
	s.put(FloatType)

	return s
}
