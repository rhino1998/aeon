package compiler

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/parser"
)

type PositionError = parser.PositionError

type FileError struct {
	File string
	Err  error
}

func (e FileError) Error() string {
	return fmt.Sprintf("%s: %v", e.File, e.Err)
}

type CompilerError struct {
	Err error
}

func (e CompilerError) Error() string {
	return fmt.Sprintf("error: %v", e.Err)
}

func (e CompilerError) Unwrap() error {
	return e.Err
}
