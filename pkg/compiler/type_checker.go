package compiler

import (
	"fmt"
	"log"
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
		err := c.resolveStatementTypes(f.symbols, stmt)
		if err != nil {
			return err
		}
	}

	return nil
}

func (c *Compiler) resolveStatementTypes(scope *SymbolScope, stmt Statement) error {
	switch stmt := stmt.(type) {
	case *VarStatement:
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
		if stmt.Condition != nil {
			cond, err := c.resolveExpressionTypes(scope, stmt.Condition, KindType(KindBool))
			if err != nil {
				return err
			}

			if cond.Type().Kind() != KindBool {
				return cond.WrapError(fmt.Errorf("cannot use expression of type %v as a condition", cond.Type()))
			}

			stmt.Condition = cond
		}

		for _, subStmt := range stmt.Body {
			err := c.resolveStatementTypes(stmt.Scope, subStmt)
			if err != nil {
				return err
			}
		}

		if stmt.Else != nil {
			err := c.resolveStatementTypes(scope, stmt.Else)
			if err != nil {
				return err
			}
		}

		return nil
	case *ForStatement:
		if stmt.Init != nil {
			err := c.resolveStatementTypes(stmt.Scope, stmt.Init)
			if err != nil {
				return err
			}
		}

		if stmt.Condition != nil {
			cond, err := c.resolveExpressionTypes(stmt.Scope, stmt.Condition, KindType(KindBool))
			if err != nil {
				return err
			}

			if cond.Type().Kind() != KindBool {
				return cond.WrapError(fmt.Errorf("cannot use expression of type %v as a condition", cond.Type()))
			}

			stmt.Condition = cond
		}

		if stmt.Step != nil {
			err := c.resolveStatementTypes(stmt.Scope, stmt.Step)
			if err != nil {
				return err
			}
		}

		for _, subStmt := range stmt.Body {
			err := c.resolveStatementTypes(stmt.Scope, subStmt)
			if err != nil {
				return err
			}
		}

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
	case *ExpressionStatement:
		expr, err := c.resolveExpressionTypes(scope, stmt.Expression, nil)
		if err != nil {
			return err
		}

		stmt.Expression = expr

		return nil
	default:
		return stmt.WrapError(fmt.Errorf("unhandled statement in type checker: %T", stmt))
	}

}

func (c *Compiler) resolveExpressionTypes(scope *SymbolScope, expr Expression, bound Type) (Expression, error) {
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
		sym := expr.Dereference()
		if sym == nil {
			return nil, expr.WrapError(fmt.Errorf("undefined name %s", expr.Name()))
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
		// TODO: operatore-aware bounds

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

		log.Printf("%T %T", expr.Left, expr.Right)
		log.Println(expr.Left.Type(), expr.Right.Type())

		typ, err := validateBinaryExpression(expr.Left.Type(), expr.Operator, expr.Right.Type())

		if err != nil {
			return nil, expr.WrapError(err)
		}

		expr.SetType(typ)

		return expr, nil
	case *UnaryExpression:
		// TODO: operatore-aware bounds

		subExpr, err := c.resolveExpressionTypes(scope, expr.Expression, bound)
		if err != nil {
			return nil, err
		}

		expr.Expression = subExpr

		return expr, nil
	case *CallExpression:
		fExpr, err := c.resolveExpressionTypes(scope, expr.Function, nil)
		if err != nil {
			return nil, err
		}

		expr.Function = fExpr

		if fExpr.Type().Kind() != KindFunction {
			return nil, expr.WrapError(fmt.Errorf("cannot call non-function type %v", fExpr.Type()))
		}

		baseFType := BaseType(fExpr.Type()).(*FunctionType)

		// TODO: variadic
		if len(baseFType.Parameters) != len(expr.Args) {
			return nil, expr.WrapError(fmt.Errorf("function call expects %d parameters, got %d", len(baseFType.Parameters), len(expr.Args)))
		} else {
			for i := range len(expr.Args) {
				arg, err := c.resolveExpressionTypes(scope, expr.Args[i], baseFType.Parameters[i])
				if err != nil {
					return nil, err
				}

				expr.Args[i] = arg
			}
		}

		return expr, nil
	default:
		return nil, expr.WrapError(fmt.Errorf("unhandled expression in type checker: %T", expr))
	}
}
