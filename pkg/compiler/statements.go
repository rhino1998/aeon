package compiler

type Statement interface {
	WrapError(err error) error
}
