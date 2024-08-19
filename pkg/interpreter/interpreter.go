package interpreter

import (
	"errors"
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler"
)

var (
	ErrReturn = errors.New("return")

	ErrContinue = errors.New("continue")
	ErrBreak    = errors.New("break")
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
	entryPkg, ok := s.prog.Package("main")
	if !ok {
		return fmt.Errorf("could not get entry point function package")
	}

	entryFunc, ok := entryPkg.Function("main")
	if !ok {
		return fmt.Errorf("could not get entry point function")
	}

	if len(entryFunc.Parameters()) != 0 {
		return fmt.Errorf("entry point function must not accept any parameters")
	}

	if entryFunc.Return() != nil {
		return fmt.Errorf("entry point function must not return a value")
	}
	global := newScope(nil, "")

	_, err := s.executeFunction(global, entryFunc)
	if err != nil {
		return err
	}

	return nil
}

func (s *State) executeFunction(scope *Scope, f *compiler.Function, args ...Value) (Value, error) {
	scope = newScope(scope, f.Name())

	params := f.Parameters()
	if len(params) != len(args) {
		return nil, fmt.Errorf("expected %d arguments, got %d", len(params), len(args))
	}

	for i := range len(args) {
		scope.Put(params[i].Name(), NewVariable(params[i].Type(), args[i]))
	}

	for _, stmt := range f.Body() {
		var ret Value
		err := s.executeStatement(scope, stmt, &ret)
		if err != nil {
			if errors.Is(err, ErrReturn) {
				if ret != nil && f.Return() == compiler.TypeVoid {
					return nil, fmt.Errorf("unexpected return value in function %s", f.Name())
				}

				return ret, nil
			}
			return nil, err
		}
	}

	if f.Return() != nil {
		return nil, fmt.Errorf("missing return statement in function %s", f.Name())
	}

	return nil, nil
}

func (s *State) executeStatement(scope *Scope, stmt compiler.Statement, ret *Value) error {
	var err error
	switch stmt := stmt.(type) {
	case *compiler.ExpressionStatement:
		_, err := s.executeExpression(scope, stmt.Expression)
		if err != nil {
			return err
		}

		return nil
	case *compiler.VarStatement:
		var val Value
		if stmt.Expression != nil {
			val, err = s.executeExpression(scope, stmt.Expression)
			if err != nil {
				return err
			}
		}

		scope.Put(stmt.Variable.Name(), NewVariable(stmt.Variable.Type(), val))

		return nil
	case *compiler.DeclarationStatement:
		var val Value
		if stmt.Expression != nil {
			val, err = s.executeExpression(scope, stmt.Expression)
			if err != nil {
				return err
			}
		}

		scope.Put(stmt.Variable.Name(), NewVariable(stmt.Variable.Type(), val))
		return nil
	case *compiler.AssignmentOperatorStatement:
		lhs, err := s.executeExpression(scope, stmt.Left)
		if err != nil {
			return err
		}

		rhs, err := s.executeExpression(scope, stmt.Right)
		if err != nil {
			return err
		}

		var op compiler.Operator
		switch stmt.Operator {
		case compiler.OperatorPlusEquals:
			op = compiler.OperatorAddition
		case compiler.OperatorMinusEquals:
			op = compiler.OperatorAddition
		default:
			return stmt.WrapError(fmt.Errorf("unsupported postfix operator: %s", stmt.Operator))
		}

		res, err := s.binaryOperate(lhs, rhs, op)
		if err != nil {
			return stmt.WrapError(err)
		}

		err = s.setOrFail(lhs, res)
		if err != nil {
			return stmt.WrapError(err)
		}

		return nil
	case *compiler.AssignmentStatement:
		lhs, err := s.executeExpression(scope, stmt.Left)
		if err != nil {
			return err
		}

		settableLHS, ok := lhs.(SettableValue)
		if !ok {
			return fmt.Errorf("assignment to non-settable value: %T", lhs)
		}

		rhs, err := s.executeExpression(scope, stmt.Right)
		if err != nil {
			return err
		}

		return settableLHS.Set(rhs)
	case *compiler.ReturnStatement:
		if stmt.Expression == nil {
			return ErrReturn
		}

		val, err := s.executeExpression(scope, stmt.Expression)
		if err != nil {
			return err
		}

		*ret = val
		return ErrReturn
	case *compiler.PostfixStatement:
		expr, err := s.executeExpression(scope, stmt.Expression)
		if err != nil {
			return err
		}

		err = s.setOrFail(expr, expr)
		if err != nil {
			return stmt.WrapError(err)
		}

		var op compiler.Operator
		switch stmt.Operator {
		case compiler.OperatorIncrement:
			op = compiler.OperatorAddition
		case compiler.OperatorDecrement:
			op = compiler.OperatorAddition
		default:
			return stmt.WrapError(fmt.Errorf("unsupported postfix operator: %s", stmt.Operator))
		}

		if expr.Type().Kind() != compiler.KindFloat && expr.Type().Kind() != compiler.KindInt {
			return stmt.WrapError(fmt.Errorf("postfix operator %q only applies to integer or float values", stmt.Operator))
		}

		res, err := s.binaryOperate(expr, NewConstant(expr.Type(), int64(1)), op)
		if err != nil {
			return stmt.WrapError(err)
		}

		err = s.setOrFail(expr, res)
		if err != nil {
			return stmt.WrapError(err)
		}

		return nil
	case *compiler.IfStatement:
		var b bool
		if stmt.Condition != nil {
			cond, err := s.executeExpression(scope, stmt.Condition)
			if err != nil {
				return err
			}

			b, err = s.boolOrFail(cond)
			if err != nil {
				return stmt.WrapError(err)
			}
		}

		if b {
			scope := newScope(scope, "if")

			for _, stmt := range stmt.Body {
				err := s.executeStatement(scope, stmt, ret)
				if err != nil {
					return err
				}
			}

			return nil
		} else if stmt.Else != nil {
			return s.executeStatement(scope, stmt.Else, ret)
		} else {
			return nil
		}
	case *compiler.ForStatement:
		scope := newScope(scope, "for")
		if stmt.Init != nil {
			err := s.executeStatement(scope, stmt.Init, ret)
			if err != nil {
				return err
			}
		}

		for {
			if stmt.Condition != nil {
				condVal, err := s.executeExpression(scope, stmt.Condition)
				if err != nil {
					return err
				}

				b, err := s.boolOrFail(condVal)
				if err != nil {
					return stmt.WrapError(err)
				}

				if !b {
					return nil
				}
			}

			for _, subStmt := range stmt.Body {
				err := s.executeStatement(scope, subStmt, ret)
				if err != nil {
					return err
				}
			}

			if stmt.Step != nil {
				err := s.executeStatement(scope, stmt.Step, ret)
				if err != nil {
					return err
				}
			}
		}
	default:
		return stmt.WrapError(fmt.Errorf("unhandled statement type: %T", stmt))
	}
}

func (s *State) executeExpression(scope *Scope, expr compiler.Expression) (Value, error) {
	switch expr := expr.(type) {
	case *compiler.Literal:
		return NewConstant(expr.Type(), expr.Value()), nil
	case *compiler.SymbolReferenceExpression:
		val, ok := scope.Get(expr.Name())
		if !ok {
			return nil, fmt.Errorf("undefined variable %s", expr.Name())
		}

		return val, nil
	case *compiler.ParenthesizedExpression:
		return s.executeExpression(scope, expr.Expression)
	case *compiler.BinaryExpression:
		switch expr.Operator {
		case compiler.OperatorLogicalAnd:
		case compiler.OperatorLogicalOr:
		default:
		}

		lhs, err := s.executeExpression(scope, expr.Left)
		if err != nil {
			return nil, err
		}

		rhs, err := s.executeExpression(scope, expr.Right)
		if err != nil {
			return nil, err
		}

		result, err := s.binaryOperate(lhs, rhs, expr.Operator)
		if err != nil {
			return nil, expr.WrapError(err)
		}

		return result, nil
	case *compiler.UnaryExpression:
		val, err := s.executeExpression(scope, expr.Expression)
		if err != nil {
			return nil, err
		}

		res, err := s.unaryOperate(val, expr.Operator)
		if err != nil {
			return nil, expr.WrapError(err)
		}

		return res, nil
	default:
		return nil, expr.WrapError(fmt.Errorf("unhandled expression type: %T", expr))
	}
}

func (s *State) setOrFail(maybeSettable, val Value) error {
	settable, ok := maybeSettable.(SettableValue)
	if !ok {
		return fmt.Errorf("assignment to non-settable value: %T", maybeSettable)
	}

	return settable.Set(val)
}

func (s *State) binaryOperate(lhs, rhs Value, op compiler.Operator) (Value, error) {
	switch op {
	case compiler.OperatorAddition:
		switch lhs.Type().Kind() {
		case compiler.KindFloat:
			lhsVal, err := s.floatOrFail(lhs)
			if err != nil {
				return nil, err
			}

			rhsVal, err := s.floatOrFail(rhs)
			if err != nil {
				return nil, err
			}

			return NewConstant(lhs.Type(), float64(lhsVal+rhsVal)), nil
		case compiler.KindInt:
			lhsVal, err := s.intOrFail(lhs)
			if err != nil {
				return nil, err
			}

			rhsVal, err := s.intOrFail(rhs)
			if err != nil {
				return nil, err
			}

			return NewConstant(lhs.Type(), int64(lhsVal+rhsVal)), nil
		case compiler.KindString:
			lhsVal, err := s.stringOrFail(lhs)
			if err != nil {
				return nil, err
			}

			rhsVal, err := s.stringOrFail(rhs)
			if err != nil {
				return nil, err
			}

			return NewConstant(lhs.Type(), string(lhsVal+rhsVal)), nil
		default:
			return nil, fmt.Errorf("unsupported binary operation: %s + %s", lhs.Type(), rhs.Type())
		}
	case compiler.OperatorEqual:
		b := s.valuesEqual(lhs, rhs)

		return NewConstant(compiler.TypeBool, b), nil
	case compiler.OperatorNotEqual:
		b := !s.valuesEqual(lhs, rhs)

		return NewConstant(compiler.TypeBool, b), nil
	case compiler.OperatorLessThan:
		switch lhs.Type().Kind() {
		case compiler.KindFloat:
			lhsVal, err := s.floatOrFail(lhs)
			if err != nil {
				return nil, err
			}

			rhsVal, err := s.floatOrFail(rhs)
			if err != nil {
				return nil, err
			}

			b := lhsVal < rhsVal

			return NewConstant(compiler.TypeBool, b), nil
		case compiler.KindInt:
			lhsVal, err := s.intOrFail(lhs)
			if err != nil {
				return nil, err
			}

			rhsVal, err := s.intOrFail(rhs)
			if err != nil {
				return nil, err
			}

			b := lhsVal < rhsVal

			return NewConstant(compiler.TypeBool, b), nil
		case compiler.KindString:
			lhsVal, err := s.stringOrFail(lhs)
			if err != nil {
				return nil, err
			}

			rhsVal, err := s.stringOrFail(rhs)
			if err != nil {
				return nil, err
			}

			b := lhsVal < rhsVal

			return NewConstant(compiler.TypeBool, b), nil
		default:
			return nil, fmt.Errorf("unsupported binary operation: %s < %s", lhs.Type(), rhs.Type())
		}
	default:
		return nil, fmt.Errorf("unsupported binary operation: %s", op)
	}
}

func (s *State) unaryOperate(val Value, op compiler.Operator) (Value, error) {
	switch op {
	case compiler.OperatorPositive:
		switch val.Type().Kind() {
		case compiler.KindFloat:
			raw := val.Raw().(float64)
			return NewConstant(val.Type(), +raw), nil
		case compiler.KindInt:
			raw := val.Raw().(int64)
			return NewConstant(val.Type(), +raw), nil
		default:
			return nil, fmt.Errorf("operator %s is not supported on type %v", op, val.Type())
		}
	case compiler.OperatorNegate:
		switch val.Type().Kind() {
		case compiler.KindFloat:
			raw := val.Raw().(float64)
			return NewConstant(val.Type(), -raw), nil
		case compiler.KindInt:
			raw := val.Raw().(int64)
			return NewConstant(val.Type(), -raw), nil
		default:
			return nil, fmt.Errorf("operator %s is not supported on type %v", op, val.Type())
		}
	default:
		return nil, fmt.Errorf("operator %s is not supported on type %v", op, val.Type())
	}
}

func (s *State) intOrFail(val Value) (int64, error) {
	if val.Type().Kind() != compiler.KindInt {
		return 0, fmt.Errorf("expected integer value, got %T", val.Type())
	}

	return val.Raw().(int64), nil
}

func (s *State) floatOrFail(val Value) (float64, error) {
	if val.Type().Kind() != compiler.KindFloat {
		return 0, fmt.Errorf("expected float value, got %T", val.Type())
	}

	return val.Raw().(float64), nil
}

func (s *State) stringOrFail(val Value) (string, error) {
	if val.Type().Kind() != compiler.KindString {
		return "", fmt.Errorf("expected string value, got %T", val.Type())
	}

	return val.Raw().(string), nil
}

func (s *State) boolOrFail(val Value) (bool, error) {
	if val.Type().Kind() != compiler.KindBool {
		return false, fmt.Errorf("expected bool value, got %T", val.Type())
	}

	return val.Raw().(bool), nil
}

func (s *State) valuesEqual(a, b Value) bool {
	if !compiler.TypesEqual(a.Type(), b.Type()) {
		return false
	}

	return a.Raw() == b.Raw() // TODO: handle complex types
}
