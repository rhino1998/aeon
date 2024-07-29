package compiler

type Kind int

const (
	KindBoolean Kind = iota
	KindInteger
	KindFloat
	KindString
	KindMap
	KindStruct
	KindArray
	KindInterface
	KindFunction
	KindBuiltin
)
