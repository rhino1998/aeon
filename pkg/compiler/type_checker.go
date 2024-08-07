package compiler

import (
	"fmt"
	"strconv"
)

func (c *Compiler) resolveProgramTypes(prog *Program) error {
	for _, pkg := range prog.Packages() {
		err := c.resolvePackageTypes(pkg)
		if err != nil {
			return err
		}
	}

	return nil
}

func (c *Compiler) resolvePackageTypes(pkg *Package) (err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	for _, f := range pkg.ExternFunctions() {
		err := c.resolveExternFunctionTypes(f)
		if err != nil {
			errs.Add(err)
		}
	}

	for _, f := range pkg.Functions() {
		err := c.resolveFunctionTypes(f)
		if err != nil {
			errs.Add(err)
		}
	}

	return nil
}

func (c *Compiler) resolveExternFunctionTypes(_ *ExternFunction) error {
	return nil
}

func (c *Compiler) resolveFunctionTypes(f *Function) (err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	for _, stmt := range f.Body() {
		err := c.resolveStatementTypes(f.symbols, stmt)
		if err != nil {
			errs.Add(err)
		}
	}

	return nil
}

func (c *Compiler) resolveStatementTypes(scope *SymbolScope, stmt Statement) (err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

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
				errs.Add(err)
			}

			if expr.Type() == VoidType {
				errs.Add(expr.WrapError(fmt.Errorf("cannot declare variable of type void")))
			}

			stmt.Expression = expr
			stmt.Variable.SetType(expr.Type())
		}

		return nil
	case *DeclarationStatement:
		expr, err := c.resolveExpressionTypes(scope, stmt.Expression, nil)
		if err != nil {
			errs.Add(err)
		}

		if expr.Type() == VoidType {
			errs.Add(expr.WrapError(fmt.Errorf("cannot declare variable of type void")))
		}

		stmt.Expression = expr
		stmt.Variable.SetType(expr.Type())

		return nil
	case *AssignmentOperatorStatement:
		left, err := c.resolveLHSTypes(scope, stmt.Left)
		if err != nil {
			errs.Add(err)
		}

		stmt.Left = left

		right, err := c.resolveExpressionTypes(scope, stmt.Right, left.Type())
		if err != nil {
			errs.Add(err)
		}

		stmt.Right = right

		// TODO: validate by operator

		return nil
	case *AssignmentStatement:
		left, err := c.resolveLHSTypes(scope, stmt.Left)
		if err != nil {
			errs.Add(err)
		}

		stmt.Left = left

		right, err := c.resolveExpressionTypes(scope, stmt.Right, left.Type())
		if err != nil {
			errs.Add(err)
		}

		stmt.Right = right

		return nil
	case *PostfixStatement:
		expr, err := c.resolveExpressionTypes(scope, stmt.Expression, nil)
		if err != nil {
			errs.Add(err)
		}
		stmt.Expression = expr

		// TODO: validate by operator

		return nil
	case *IfStatement:
		if stmt.Condition != nil {
			cond, err := c.resolveExpressionTypes(scope, stmt.Condition, KindType(KindBool))
			if err != nil {
				errs.Add(err)
			}

			if cond.Type().Kind() != KindBool {
				errs.Add(cond.WrapError(fmt.Errorf("cannot use expression of type %v as a condition", cond.Type())))
			}

			stmt.Condition = cond
		}

		for _, subStmt := range stmt.Body {
			err := c.resolveStatementTypes(stmt.Scope, subStmt)
			if err != nil {
				errs.Add(err)
			}
		}

		if stmt.Else != nil {
			err := c.resolveStatementTypes(scope, stmt.Else)
			if err != nil {
				errs.Add(err)
			}
		}

		return nil
	case *ForStatement:
		if stmt.Init != nil {
			err := c.resolveStatementTypes(stmt.Scope, stmt.Init)
			if err != nil {
				errs.Add(err)
			}
		}

		if stmt.Condition != nil {
			cond, err := c.resolveExpressionTypes(stmt.Scope, stmt.Condition, KindType(KindBool))
			if err != nil {
				errs.Add(err)
			}

			if cond.Type().Kind() != KindBool {
				errs.Add(cond.WrapError(fmt.Errorf("cannot use expression of type %v as a condition", cond.Type())))
			}

			stmt.Condition = cond
		}

		if stmt.Step != nil {
			err := c.resolveStatementTypes(stmt.Scope, stmt.Step)
			if err != nil {
				errs.Add(err)
			}
		}

		for _, subStmt := range stmt.Body {
			err := c.resolveStatementTypes(stmt.Scope, subStmt)
			if err != nil {
				errs.Add(err)
			}
		}

		return nil
	case *ReturnStatement:
		if scope.Function() == nil {
			errs.Add(stmt.WrapError(fmt.Errorf("return statement outside of function")))
		}

		if stmt.Expression != nil && scope.Function().Return() == VoidType {
			errs.Add(stmt.WrapError(fmt.Errorf("unexpected return value on void function")))
		}

		if stmt.Expression == nil && scope.Function().Return() != VoidType {
			errs.Add(stmt.WrapError(fmt.Errorf("expected return value of type %s", scope.Function().Return())))
		}

		if stmt.Expression != nil {
			expr, err := c.resolveExpressionTypes(scope, stmt.Expression, scope.Function().Return())
			if err != nil {
				errs.Add(err)
			}

			stmt.Expression = expr
		}

		return nil
	case *ExpressionStatement:
		expr, err := c.resolveExpressionTypes(scope, stmt.Expression, nil)
		if err != nil {
			errs.Add(err)
		}

		stmt.Expression = expr

		return nil
	default:
		return stmt.WrapError(fmt.Errorf("unhandled statement in type checker: %T", stmt))
	}
}

func (c *Compiler) resolveLHSTypes(scope *SymbolScope, expr Expression) (_ Expression, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch expr := expr.(type) {
	case *Literal[Int]:
		return expr, expr.WrapError(fmt.Errorf("cannot assign to int literal"))
	case *Literal[Float]:
		return expr, expr.WrapError(fmt.Errorf("cannot assign to float literal"))
	case *Literal[String]:
		return expr, expr.WrapError(fmt.Errorf("cannot assign to string literal"))
	case *Literal[Bool]:
		return expr, expr.WrapError(fmt.Errorf("cannot assign to bool literal"))
	case *SymbolReferenceExpression:
		sym, ok := expr.scope.get(expr.Name())
		if !ok {
			return expr, expr.WrapError(fmt.Errorf("cannot assign to unrecognized symbol %q", expr.Name()))
		}

		switch sym.(type) {
		case *Function:
			return expr, expr.WrapError(fmt.Errorf("cannot assign to function %q", expr.Name()))
		case *ExternFunction:
			return expr, expr.WrapError(fmt.Errorf("cannot assign to extern function %q", expr.Name()))
		case *Package:
			return expr, expr.WrapError(fmt.Errorf("cannot assign to package %q", expr.Name()))
		case *Variable:
			return expr, nil
		default:
			return expr, expr.WrapError(fmt.Errorf("cannot assign to expression type: %T", expr))
		}
	case *ParenthesizedExpression:
		return c.resolveLHSTypes(scope, expr.Expression)
	case *BinaryExpression:
		return expr, expr.WrapError(fmt.Errorf("cannot assign to binary expression"))
	case *UnaryExpression:
		// TODO: deref
		return expr, expr.WrapError(fmt.Errorf("cannot assign to unary expression"))
	case *DotExpression:
		receiver, err := c.resolveExpressionTypes(scope, expr.Receiver, nil)
		if err != nil {
			errs.Add(err)
		}

		expr.Receiver = receiver
		switch typ := BaseType(expr.Receiver.Type()).(type) {
		case *TupleType:
			index, err := strconv.Atoi(expr.Key)
			if err != nil {
				errs.Add(expr.WrapError(fmt.Errorf("cannot index tuple with %q", expr.Key)))
			}

			if index >= len(typ.Elems()) {
				errs.Add(expr.WrapError(fmt.Errorf("tuple index %d out bounds on tuple with %d elements", index, len(typ.Elems()))))
			}

			return expr, nil
		default:
			return expr, expr.WrapError(fmt.Errorf("cannot dot index assign receiver type: %T", expr.Receiver))
		}
	default:
		return expr, expr.WrapError(fmt.Errorf("cannot assign to expression type: %T", expr))
	}
}

func (c *Compiler) resolveExpressionTypes(scope *SymbolScope, expr Expression, bound Type) (_ Expression, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch expr := expr.(type) {
	case *Literal[Int]:
		if bound == nil && IsUnspecified(expr.Type()) {
			return &Literal[Int]{
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
			return &Literal[Float]{
				value: Float(expr.value),
				typ:   bound,

				Position: expr.Position,
			}, nil
		case KindInt:
			if IsUnspecified(bound) {
				bound = IntType
			}

			return &Literal[Int]{
				value: expr.value,
				typ:   bound,

				Position: expr.Position,
			}, nil
		default:
			return nil, expr.WrapError(fmt.Errorf("cannot coerce int literal into %s", bound))
		}
	case *Literal[Float]:
		if bound == nil && IsUnspecified(expr.Type()) {
			return &Literal[Float]{
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
			return &Literal[Float]{
				value: expr.value,
				typ:   bound,

				Position: expr.Position,
			}, nil
		case KindInt:
			if IsUnspecified(bound) {
				bound = IntType
			}

			return &Literal[Int]{
				value: Int(expr.value),
				typ:   bound,

				Position: expr.Position,
			}, nil
		default:
			return nil, expr.WrapError(fmt.Errorf("cannot coerce float literal into %s", bound))
		}
	case *Literal[String]:
		if bound == nil && IsUnspecified(expr.Type()) {
			return &Literal[String]{
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
			return &Literal[String]{
				value: expr.value,
				typ:   bound,

				Position: expr.Position,
			}, nil
		default:
			return nil, expr.WrapError(fmt.Errorf("cannot coerce string literal into %s", bound))
		}
	case *Literal[Bool]:
		if bound == nil && IsUnspecified(expr.Type()) {
			return &Literal[Bool]{
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
			return &Literal[Bool]{
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
			errs.Add(expr.WrapError(fmt.Errorf("undefined name %s", expr.Name())))
		}

		return expr, nil
	case *ParenthesizedExpression:
		subExpr, err := c.resolveExpressionTypes(scope, expr.Expression, bound)
		if err != nil {
			errs.Add(err)
		}

		expr.Expression = subExpr

		return expr, nil
	case *BinaryExpression:
		// TODO: operator-aware bounds

		left, err := c.resolveExpressionTypes(scope, expr.Left, nil)
		if err != nil {
			errs.Add(err)
		}

		expr.Left = left

		right, err := c.resolveExpressionTypes(scope, expr.Right, expr.Left.Type())
		if err != nil {
			errs.Add(err)
		}

		expr.Right = right

		typ, err := validateBinaryExpression(expr.Left.Type(), expr.Operator, expr.Right.Type())

		if err != nil {
			errs.Add(expr.WrapError(err))
		}

		expr.SetType(typ)

		return expr, nil
	case *UnaryExpression:
		// TODO: operatore-aware bounds

		subExpr, err := c.resolveExpressionTypes(scope, expr.Expression, bound)
		if err != nil {
			errs.Add(err)
		}

		expr.Expression = subExpr

		return expr, nil
	case *CallExpression:
		fExpr, err := c.resolveExpressionTypes(scope, expr.Function, nil)
		if err != nil {
			errs.Add(err)
		}

		expr.Function = fExpr

		if fExpr.Type().Kind() != KindFunction {
			errs.Add(expr.WrapError(fmt.Errorf("cannot call non-function type %v", fExpr.Type())))
		}

		baseFType := BaseType(fExpr.Type()).(*FunctionType)

		if len(baseFType.Parameters) != len(expr.Args) {
			errs.Add(expr.WrapError(fmt.Errorf("function call expects %d parameters, got %d", len(baseFType.Parameters), len(expr.Args))))
		}

		for i := range min(len(expr.Args), len(baseFType.Parameters)) {
			arg, err := c.resolveExpressionTypes(scope, expr.Args[i], baseFType.Parameters[i])
			if err != nil {
				errs.Add(err)
			}

			expr.Args[i] = arg
		}

		return expr, nil
	case *DotExpression:
		receiver, err := c.resolveExpressionTypes(scope, expr.Receiver, nil)
		if err != nil {
			errs.Add(err)
		}
		expr.Receiver = receiver

		switch typ := BaseType(expr.Receiver.Type()).(type) {
		case *TupleType:
			index, err := strconv.Atoi(expr.Key)
			if err != nil {
				return expr, expr.WrapError(fmt.Errorf("cannot index tuple with %q", expr.Key))
			}

			if index >= len(typ.Elems()) {
				return expr, expr.WrapError(fmt.Errorf("tuple index %d out bounds on tuple with %d elements", index, len(typ.Elems())))
			}

			return expr, nil
		default:
			return expr, expr.WrapError(fmt.Errorf("cannot dot index receiver type %T", typ))
		}
	default:
		return expr, expr.WrapError(fmt.Errorf("unhandled expression in type checker: %T", expr))
	}
}
