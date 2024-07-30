package compiler

type Expression interface {
	Type() Type
}

type DotExpression struct {
	Receiver Expression
	Key      string
}

func (e *DotExpression) Type() Type {
	switch typ := resolveType(e.Receiver.Type()).(type) {
	case *StructType:
		return UnknownType
	case *MapType:
		return typ.Value()
	default:
		return UnknownType
	}
}
