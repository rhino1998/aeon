package interpreter

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler"
)

func Execute(prog *compiler.Program, entryPoint string) error {
	state := newState(prog, entryPoint)

	return state.execute()
}

type State struct {
	prog       *compiler.Program
	entryPoint string
}

func newState(prog *compiler.Program, entryPoint string) *State {
	return &State{
		prog:       prog,
		entryPoint: entryPoint,
	}
}

func (s *State) execute() error {
	entryFunc, err := s.prog.Function(s.entryPoint)
	if err != nil {
		return err
	}

	if len(entryFunc.Parameters()) != 0 {
		return fmt.Errorf("entry point function must not accept any parameters")
	}

	if entryFunc.Return() != nil {
		return fmt.Errorf("entry point function must not return a value")
	}
	global := newScope(nil, "")

	_, err = s.executeFunction(global, entryFunc)
	if err != nil {
		return err
	}

	return nil
}

func (s *State) executeFunction(scope *Scope, f *compiler.Function, args ...Value) (any, error) {
	scope = newScope(scope, f.Name())

	params := f.Parameters()
	if len(params) != len(args) {
		return nil, fmt.Errorf("expected %d arguments, got %d", len(params), len(args))
	}

	for i := range len(args) {
		scope.Put(params[i].Name(), args[i])
	}

	for _, stmt := range f.Body() {
		err := s.executeStatement(scope, stmt)
		if err != nil {
			return nil, err
		}
	}

	return nil, nil
}

func (s *State) executeStatement(scope *Scope, stmt compiler.Statement) error {
	switch stmt := stmt.(type) {
	case *compiler.VariableStatement:
		return nil
	case *compiler.DeclarationStatement:
		return nil
	default:
		return fmt.Errorf("unhandled statement type: %T", stmt)
	}
}
