package types

type Name string

type Namespace interface {
	QualifiedName() string
}
