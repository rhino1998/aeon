package compiler

var (
	IntType = &BasicType{
		kind: KindInteger,
		name: "int",
	}

	BoolType = &BasicType{
		kind: KindBoolean,
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

func builtins() *Scope {
	s := newScope(nil, "builtins")

	s.put(IntType)
	s.put(BoolType)
	s.put(StringType)
	s.put(FloatType)

	return s
}
