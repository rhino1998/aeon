package compiler

import (
	"fmt"
)

func (c *Compiler) resolveProgramTypes(prog *Program) error {
	for _, f := range prog.Functions() {
		err := c.resolveFunctionTypes(f)
		if err != nil {
			return err
		}
	}

	return nil
}

func (c *Compiler) resolveFunctionTypes(f *Function) error {
	for _, stmt := range f.Body() {
		err := c.resolveStatementTypes(f.scope, stmt)
		if err != nil {
			return err
		}
	}

	return nil
}

func (c *Compiler) resolveStatementTypes(scope *Scope, stmt Statement) error {
	switch stmt := stmt.(type) {
	case *VariableStatement:
		if stmt.Type != nil {
			typ := resolveType(stmt.Type)

			if kindType, ok := typ.(KindType); ok {
				switch kindType.Kind() {
				case KindInt:
					typ = IntType
				case KindFloat:
					typ = FloatType
				case KindString:
					typ = StringType
				case KindBool:
					typ = BoolType
				}
			}

			if !IsTypeResolvable(scope, typ) {
				return stmt.WrapError(fmt.Errorf("type %s is unknown", typ))
			}

			stmt.Type = typ
		}

		if stmt.Expression != nil {
			expr, err := c.resolveExpressionTypes(scope, stmt.Expression, stmt.Type)
			if err != nil {
				return err
			}

			stmt.Expression = expr
			stmt.Variable.SetType(expr.Type())
		}

		return nil
	case *DeclarationStatement:
		expr, err := c.resolveExpressionTypes(scope, stmt.Expression, nil)
		if err != nil {
			return err
		}

		stmt.Expression = expr
		stmt.Variable.SetType(expr.Type())

		return nil
	case *AssignmentOperatorStatement:
		left, err := c.resolveExpressionTypes(scope, stmt.Left, nil)
		if err != nil {
			return err
		}

		stmt.Left = left

		right, err := c.resolveExpressionTypes(scope, stmt.Right, left.Type())
		if err != nil {
			return err
		}

		stmt.Right = right

		// TODO: validate by operator

		return nil
	case *AssignmentStatement:
		left, err := c.resolveExpressionTypes(scope, stmt.Left, nil)
		if err != nil {
			return err
		}

		stmt.Left = left

		right, err := c.resolveExpressionTypes(scope, stmt.Right, left.Type())
		if err != nil {
			return err
		}

		stmt.Right = right

		return nil
	case *PostfixStatement:
		expr, err := c.resolveExpressionTypes(scope, stmt.Expression, nil)
		if err != nil {
			return err
		}
		stmt.Expression = expr

		// TODO: validate by operator

		return nil
	case *IfStatement:
		cond, err := c.resolveExpressionTypes(scope, stmt.Condition, KindType(KindBool))
		if err != nil {
			return err
		}

		if cond.Type().Kind() != KindBool {
			return cond.WrapError(fmt.Errorf("cannot use expression of type %v as a condition", cond.Type()))
		}

		stmt.Condition = cond

		return nil
	case *ReturnStatement:
		if scope.Function() == nil {
			return stmt.WrapError(fmt.Errorf("return statement outside of function"))
		}

		if stmt.Expression != nil && scope.Function().Return() == nil {
			return stmt.WrapError(fmt.Errorf("unexpected return value on void function"))
		}

		if stmt.Expression == nil && scope.Function().Return() != nil {
			return stmt.WrapError(fmt.Errorf("expected return value of type %s", scope.Function().Return()))
		}

		if stmt.Expression == nil {
			return nil
		}

		expr, err := c.resolveExpressionTypes(scope, stmt.Expression, scope.Function().Return())
		if err != nil {
			return err
		}

		stmt.Expression = expr

		return nil
	default:
		return stmt.WrapError(fmt.Errorf("unhandled statement in type checker: %T", stmt))
	}

}

func (c *Compiler) resolveExpressionTypes(scope *Scope, expr Expression, bound Type) (Expression, error) {
	switch expr := expr.(type) {
	case *Literal[int64]:
		if bound == nil && IsUnspecified(expr.Type()) {
			return &Literal[int64]{
				value: expr.value,
				typ:   IntType,

				Position: expr.Position,
			}, nil
		}

		switch bound.Kind() {
		case KindFloat:
			if IsUnspecified(bound) {
				bound = FloatType
			}

			expr.typ = bound
			return &Literal[float64]{
				value: float64(expr.value),
				typ:   bound,

				Position: expr.Position,
			}, nil
		case KindInt:
			if IsUnspecified(bound) {
				bound = IntType
			}

			return &Literal[int64]{
				value: expr.value,
				typ:   bound,

				Position: expr.Position,
			}, nil
		default:
			return nil, expr.WrapError(fmt.Errorf("cannot coerce int literal into %s", bound))
		}
	case *Literal[float64]:
		if bound == nil && IsUnspecified(expr.Type()) {
			return &Literal[float64]{
				value: expr.value,
				typ:   FloatType,

				Position: expr.Position,
			}, nil
		}

		switch bound.Kind() {
		case KindFloat:
			if IsUnspecified(bound) {
				bound = FloatType
			}

			expr.typ = bound
			return &Literal[float64]{
				value: expr.value,
				typ:   bound,

				Position: expr.Position,
			}, nil
		case KindInt:
			if IsUnspecified(bound) {
				bound = IntType
			}

			return &Literal[int64]{
				value: int64(expr.value),
				typ:   bound,

				Position: expr.Position,
			}, nil
		default:
			return nil, expr.WrapError(fmt.Errorf("cannot coerce float literal into %s", bound))
		}
	case *Literal[string]:
		if bound == nil && IsUnspecified(expr.Type()) {
			return &Literal[string]{
				value: expr.value,
				typ:   StringType,

				Position: expr.Position,
			}, nil
		}

		switch bound.Kind() {
		case KindString:
			if IsUnspecified(bound) {
				bound = StringType
			}

			expr.typ = bound
			return &Literal[string]{
				value: expr.value,
				typ:   bound,

				Position: expr.Position,
			}, nil
		default:
			return nil, expr.WrapError(fmt.Errorf("cannot coerce string literal into %s", bound))
		}
	case *Literal[bool]:
		if bound == nil && IsUnspecified(expr.Type()) {
			return &Literal[bool]{
				value: expr.value,
				typ:   StringType,

				Position: expr.Position,
			}, nil
		}

		switch bound.Kind() {
		case KindBool:
			if IsUnspecified(bound) {
				bound = BoolType
			}
			expr.typ = bound
			return &Literal[bool]{
				value: expr.value,
				typ:   bound,

				Position: expr.Position,
			}, nil
		default:
			return nil, expr.WrapError(fmt.Errorf("cannot coerce bool literal into %s", bound))
		}
	case *SymbolReferenceExpression:
		if bound != nil {
			sym := expr.Dereference()
			if sym == nil {
				return nil, expr.WrapError(fmt.Errorf("undefined name %s", expr.Name()))
			}
		}

		return expr, nil
	case *ParenthesizedExpression:
		subExpr, err := c.resolveExpressionTypes(scope, expr.Expression, bound)
		if err != nil {
			return nil, err
		}

		expr.Expression = subExpr

		return expr, nil
	case *BinaryExpression:
		left, err := c.resolveExpressionTypes(scope, expr.Left, nil)
		if err != nil {
			return nil, err
		}

		expr.Left = left

		right, err := c.resolveExpressionTypes(scope, expr.Right, expr.Left.Type())
		if err != nil {
			return nil, err
		}

		expr.Right = right

		typ, err := validateBinaryExpression(expr.Left.Type(), expr.Operator, expr.Right.Type())

		if err != nil {
			return nil, expr.WrapError(err)
		}

		expr.SetType(typ)

		return expr, nil
	default:
		return nil, expr.WrapError(fmt.Errorf("unhandled expression in type checker: %T", expr))
	}
}
