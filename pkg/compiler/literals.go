package compiler

type BooleanLiteral struct {
	value bool
	typ   Type
}

func (l BooleanLiteral) Type() Type {
	return l.typ
}

type NumericLiteral struct {
	value float64
	typ   Type
}

func (l NumericLiteral) Type() Type {
	return l.typ
}

type StringLiteral struct {
	value string
	typ   Type
}

func (l StringLiteral) Type() Type {
	return l.typ
}
