package compiler

import (
	"errors"
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

type ErrorSet struct {
	Errs    []error
	wrapper func(error) error
}

func newErrorSet() *ErrorSet {
	return new(ErrorSet)
}

func newErrorSetWithWrapper(wrapper func(error) error) *ErrorSet {
	return &ErrorSet{
		wrapper: wrapper,
	}
}

func (e *ErrorSet) wrap(err error) error {
	if e.wrapper == nil {
		return err
	}

	return e.wrapper(err)
}

func (e *ErrorSet) Add(err error) {
	if err == nil {
		return
	}
	var subErrs *ErrorSet
	if errors.As(err, &subErrs) {
		var errs []error
		for _, subErr := range subErrs.Unwrap() {
			errs = append(errs, e.wrap(subErr))
		}
		e.Errs = append(e.Errs, errs...)
	} else {
		e.Errs = append(e.Errs, err)
	}
}

func (e ErrorSet) Error() string {
	return errors.Join(e.Errs...).Error()
}

func (e ErrorSet) Unwrap() []error {
	return e.Errs
}

func (e *ErrorSet) Defer(err error) error {
	if err != nil && e != err {
		e.Add(err)
	}

	if len(e.Errs) == 0 {
		return nil
	}

	return e
}

type ErrorWrapper interface {
	WrapError(err error) error
}
