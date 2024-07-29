package compiler

type Program struct {
	Package Package
}

type Package struct {
	Name string
}

type BooleanLiteral bool

type Type interface {
}

type TypeIdentifier string

type Identifier string
