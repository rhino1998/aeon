package compiler

import (
	"fmt"
	"strconv"

	"github.com/rhino1998/aeon/pkg/compiler/air"
	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/operators"
	"github.com/rhino1998/aeon/pkg/compiler/types"
	"github.com/rhino1998/aeon/pkg/parser"
)

func (c *Compiler) resolveProgramTypes(prog *Program) (err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()
	for _, pkg := range prog.Packages() {
		err := c.resolvePackageTypes(pkg)
		if err != nil {
			errs.Add(err)
		}
	}

	return nil
}

func (c *Compiler) resolvePackageTypes(pkg *Package) (err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	for _, typ := range pkg.DerivedTypes() {
		newTyp, err := c.resolveTypes(typ.Position, typ.Underlying())
		if err != nil {
			errs.Add(err)
		}

		typ.SetUnderlying(newTyp)
	}

	for _, g := range pkg.Globals() {
		err := c.resolveGlobalTypes(pkg, g)
		if err != nil {
			errs.Add(err)
		}
	}

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

	f.receiver.typ, err = c.resolveTypes(f.Position, f.Receiver().Type())
	if err != nil {
		errs.Add(err)
	}

	if f.Receiver().Type().Kind() == kinds.Unknown {
		errs.Add(f.WrapError(fmt.Errorf("function %q has unknown receiver %q of type %s", f.Name(), f.Receiver().Name(), f.Receiver().Type())))
	}

	for _, param := range f.Parameters() {
		param.typ, err = c.resolveTypes(param.Position, param.Type())
		if err != nil {
			errs.Add(err)
		}

		if param.Type().Kind() == kinds.Unknown {
			errs.Add(f.WrapError(fmt.Errorf("function %q has unknown parameter %q of type %s", f.Name(), param.Name(), param.Type())))
		}
	}

	f.ret, err = c.resolveTypes(f.Position, f.Return())
	if err != nil {
		errs.Add(err)
	}

	if f.Return().Kind() == kinds.Unknown {
		errs.Add(f.WrapError(fmt.Errorf("function %q has unknown return type %s", f.Name(), f.Return())))
	}

	for _, stmt := range f.Body() {
		err := c.resolveStatementTypes(stmt)
		if err != nil {
			errs.Add(err)
		}
	}

	return nil
}

func (c *Compiler) resolveTypes(pos parser.Position, typ types.Type) (_ types.Type, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	if !types.IsResolvable(typ) {
		errs.Add(pos.WrapError(fmt.Errorf("type %s is unknown", typ)))
	}

	switch typ := typ.(type) {
	case *types.Array:
		return typ, nil
	case types.Kind:
		switch typ.Kind() {
		case kinds.Int:
			return TypeInt, nil
		case kinds.Float:
			return TypeFloat, nil
		case kinds.String:
			return TypeString, nil
		case kinds.Bool:
			return TypeBool, nil
		default:
			return typ, nil
		}
	default:
		return typ, nil
	}
}

func (c *Compiler) resolveGlobalTypes(pkg *Package, g *Variable) (err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	if g.Type() != nil {
		typ := types.Dereference(g.Type())

		g.typ, err = c.resolveTypes(g.Position, typ)
		if err != nil {
			return err
		}

		if !types.IsResolvable(typ) {
			errs.Add(g.WrapError(fmt.Errorf("type %s is unknown", typ)))
		}

		g.SetType(typ)
	}

	if g.expr != nil {
		expr, err := c.resolveExpressionTypes(g.expr, g.Type())
		if err != nil {
			errs.Add(err)
		}

		g.expr = expr
	}

	return nil
}

func (c *Compiler) resolveStatementTypes(stmt Statement) (err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch stmt := stmt.(type) {
	case *VarStatement:
		if stmt.Type != nil {
			typ := types.Dereference(stmt.Type)

			stmt.Type, err = c.resolveTypes(stmt.Position, typ)
			if err != nil {
				return err
			}

			stmt.Type = typ
			stmt.Variable.SetType(stmt.Type)
		}

		if stmt.Expression != nil {
			expr, err := c.resolveExpressionTypes(stmt.Expression, stmt.Type)
			if err != nil {
				errs.Add(err)
			}

			if expr.Type() == types.Void {
				errs.Add(expr.WrapError(fmt.Errorf("cannot declare variable of type void")))
			}

			if expr.Type().Kind() == kinds.Type {
				errs.Add(expr.WrapError(fmt.Errorf("cannot use type as a value")))
			}

			if !types.IsAssignableTo(expr.Type(), stmt.Type) {
				errs.Add(stmt.WrapError(fmt.Errorf("cannot assign type %v to variable %q of type %v", expr.Type(), stmt.Variable.Name(), stmt.Type)))
			}

			if stmt.Type.Kind() == kinds.Interface && expr.Type().Kind() != kinds.Interface {
				stmt.Expression = &InterfaceCoercionExpression{
					Interface:  types.Resolve(stmt.Type).(*types.Interface),
					Expression: expr,
				}
			} else {
				stmt.Expression = expr
			}

		}

		return nil
	case *DeclarationStatement:
		expr, err := c.resolveExpressionTypes(stmt.Expression, nil)
		if err != nil {
			errs.Add(err)
		}

		if expr.Type() == types.Void {
			errs.Add(expr.WrapError(fmt.Errorf("cannot declare variable of type void")))
		}

		if expr.Type().Kind() == kinds.Type {
			errs.Add(expr.WrapError(fmt.Errorf("cannot use type as a value")))
		}

		stmt.Expression = expr

		if len(stmt.Variables) > 1 {
			tupleType, ok := types.Resolve(expr.Type()).(*types.Tuple)
			if !ok {
				errs.Add(stmt.WrapError(fmt.Errorf("cannot destructure non-tuple type %s", expr.Type())))
			} else {
				for i := range stmt.Variables {
					var varType types.Type = types.Unknown
					if i < len(tupleType.Elems()) {
						varType = tupleType.Elems()[i]
					}

					stmt.Variables[i].SetType(varType)
				}
			}
		} else {
			stmt.Variables[0].SetType(expr.Type())
		}

		return nil
	case *AssignmentOperatorStatement:
		left, err := c.resolveLHSTypes(stmt.Left, stmt.Right.Type())
		if err != nil {
			errs.Add(err)
		}

		stmt.Left = left

		right, err := c.resolveExpressionTypes(stmt.Right, left.Type())
		if err != nil {
			errs.Add(err)
		}

		stmt.Right = right

		op, err := stmt.Operator.AssignmentToInfix()
		if err != nil {
			errs.Add(stmt.WrapError(err))
		}

		_, err = air.ValidateBinaryExpression(left.Type(), op, right.Type())
		if err != nil {
			errs.Add(stmt.WrapError(err))
		}

		return nil
	case *AssignmentStatement:
		right, err := c.resolveExpressionTypes(stmt.Right, stmt.Left.Type())
		if err != nil {
			errs.Add(err)
		}

		stmt.Right = right

		left, err := c.resolveLHSTypes(stmt.Left, stmt.Right.Type())
		if err != nil {
			errs.Add(err)
		}

		stmt.Left = left

		if !types.IsAssignableTo(right.Type(), left.Type()) {
			errs.Add(stmt.WrapError(fmt.Errorf("cannot assign type %v to %q of type %v", right.Type(), left, left.Type())))
		}

		if stmt.Left.Type().Kind() == kinds.Interface && stmt.Right.Type().Kind() != kinds.Interface {
			stmt.Right = &InterfaceCoercionExpression{
				Interface:  types.Resolve(stmt.Left.Type()).(*types.Interface),
				Expression: stmt.Right,
			}
		}

		return nil
	case *PostfixStatement:
		expr, err := c.resolveExpressionTypes(stmt.Expression, nil)
		if err != nil {
			errs.Add(err)
		}
		stmt.Expression = expr

		_, err = air.ValidatePostfixExpression(expr.Type(), stmt.Operator)
		if err != nil {
			errs.Add(stmt.WrapError(err))
		}

		return nil
	case *IfStatement:
		if stmt.Condition != nil {
			cond, err := c.resolveExpressionTypes(stmt.Condition, types.Kind(kinds.Bool))
			if err != nil {
				errs.Add(err)
			}

			if cond.Type().Kind() != kinds.Bool {
				errs.Add(cond.WrapError(fmt.Errorf("cannot use expression of type %v as a condition", cond.Type())))
			}

			stmt.Condition = cond
		}

		for _, subStmt := range stmt.Body {
			err := c.resolveStatementTypes(subStmt)
			if err != nil {
				errs.Add(err)
			}
		}

		if stmt.Else != nil {
			err := c.resolveStatementTypes(stmt.Else)
			if err != nil {
				errs.Add(err)
			}
		}

		return nil
	case *ForStatement:
		if stmt.Init != nil {
			err := c.resolveStatementTypes(stmt.Init)
			if err != nil {
				errs.Add(err)
			}
		}

		if stmt.Condition != nil {
			cond, err := c.resolveExpressionTypes(stmt.Condition, types.Kind(kinds.Bool))
			if err != nil {
				errs.Add(err)
			}

			if cond.Type().Kind() != kinds.Bool {
				errs.Add(cond.WrapError(fmt.Errorf("cannot use expression of type %v as a condition", cond.Type())))
			}

			stmt.Condition = cond
		}

		if stmt.Step != nil {
			err := c.resolveStatementTypes(stmt.Step)
			if err != nil {
				errs.Add(err)
			}
		}

		for _, subStmt := range stmt.Body {
			err := c.resolveStatementTypes(subStmt)
			if err != nil {
				errs.Add(err)
			}
		}

		return nil
	case *ReturnStatement:
		if stmt.Expression == nil && stmt.Function.Return() != types.Void {
			errs.Add(stmt.WrapError(fmt.Errorf("expected return value of type %s", stmt.Function.Return())))
		}

		if stmt.Expression != nil {
			expr, err := c.resolveExpressionTypes(stmt.Expression, stmt.Function.Return())
			if err != nil {
				errs.Add(err)
			}

			stmt.Expression = expr
		}

		return nil
	case *ExpressionStatement:
		expr, err := c.resolveExpressionTypes(stmt.Expression, nil)
		if err != nil {
			errs.Add(err)
		}

		stmt.Expression = expr

		return nil
	default:
		return stmt.WrapError(fmt.Errorf("unhandled statement in type checker: %T", stmt))
	}
}

func (c *Compiler) resolveLHSTypes(expr Expression, bound types.Type) (_ Expression, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch expr := expr.(type) {
	case *Literal:
		return expr, expr.WrapError(fmt.Errorf("cannot assign to literal"))
	case *SymbolReferenceExpression:
		if expr.Name() == "_" {
			return &DiscardExpression{
				typ:      bound,
				Position: expr.Position,
			}, nil
		}

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
		case *Constant:
			return expr, expr.WrapError(fmt.Errorf("cannot assign to constant %q", expr.Name()))
		default:
			return expr, expr.WrapError(fmt.Errorf("cannot assign to expression type: %T", expr))
		}
	case *ParenthesizedExpression:
		expr.Expression, err = c.resolveLHSTypes(expr.Expression, bound)
		return expr, err
	case *BinaryExpression:
		return expr, expr.WrapError(fmt.Errorf("cannot assign to binary expression"))
	case *UnaryExpression:
		switch expr.Operator {
		case operators.Dereference:
			subExpr, err := c.resolveExpressionTypes(expr.Expression, nil)
			if err != nil {
				errs.Add(err)
			}

			expr.Expression = subExpr
			return expr, nil
		default:
			return expr, expr.WrapError(fmt.Errorf("cannot assign to unary expression"))
		}
	case *DotExpression:
		receiver, err := c.resolveExpressionTypes(expr.Receiver, nil)
		if err != nil {
			errs.Add(err)
		}

		expr.Receiver = receiver

		return c.resolveLHSDotExpressionReceiverTypes(expr, expr.Receiver.Type())
	case *IndexExpression:
		receiver, err := c.resolveExpressionTypes(expr.Receiver, nil)
		if err != nil {
			errs.Add(err)
		}

		expr.Receiver = receiver

		return c.resolveLHSIndexExpressionReceiverTypes(expr, expr.Receiver.Type())
	case *TupleExpression:
		tupleBound, ok := bound.(*types.Tuple)
		if !ok {
			if !types.IsUnspecified(bound) {
				return expr, expr.WrapError(fmt.Errorf("cannot use tuple literal as %s", bound))
			} else {
				elemBounds := make([]types.Type, len(expr.Elems))
				for i := range expr.Elems {
					elemBounds[i] = types.Unknown
				}
				tupleBound = types.NewTuple(elemBounds...)
			}
		}

		if len(tupleBound.Elems()) != len(expr.Elems) {
			return expr, expr.WrapError(fmt.Errorf("expected tuple of %d elements, got %d", len(tupleBound.Elems()), len(expr.Elems)))
		}

		for i, elem := range expr.Elems {
			elem, err := c.resolveLHSTypes(elem, tupleBound.Elems()[i])
			if err != nil {
				errs.Add(err)
			}

			expr.Elems[i] = elem
		}

		return expr, nil
	default:
		return expr, expr.WrapError(fmt.Errorf("cannot assign to expression type: %T", expr))
	}
}

func (c *Compiler) resolveLHSDotExpressionReceiverTypes(expr *DotExpression, typ types.Type) (_ Expression, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch typ := types.Resolve(typ).(type) {
	case *types.Tuple:
		index, err := strconv.Atoi(expr.Key)
		if err != nil {
			errs.Add(expr.WrapError(fmt.Errorf("cannot index tuple with %q", expr.Key)))
		}

		if index >= len(typ.Elems()) {
			errs.Add(expr.WrapError(fmt.Errorf("tuple index %d out bounds on tuple with %d elements", index, len(typ.Elems()))))
		}

		return expr, nil
	case *types.Struct:
		if !typ.HasField(expr.Key) {
			errs.Add(expr.WrapError(fmt.Errorf("struct does not have field %q", expr.Key)))
		}

		return expr, nil
	case *types.Pointer:
		return c.resolveLHSDotExpressionReceiverTypes(expr, typ.Pointee())
	default:
		return expr, expr.WrapError(fmt.Errorf("cannot dot index assign receiver type: %s", expr.Receiver.Type()))
	}
}

func (c *Compiler) resolveLHSIndexExpressionReceiverTypes(expr *IndexExpression, typ types.Type) (_ Expression, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch typ := typ.(type) {
	case *types.Array:
		index, err := c.resolveExpressionTypes(expr.Index, nil)
		if err != nil {
			errs.Add(err)
		}

		if index.Type().Kind() != kinds.Int {
			errs.Add(expr.WrapError(fmt.Errorf("cannot index type %s with non-integer type %s", typ, index.Type())))
		}

		expr.Index = index

		return expr, nil
	case *types.Slice:
		index, err := c.resolveExpressionTypes(expr.Index, nil)
		if err != nil {
			errs.Add(err)
		}

		if index.Type().Kind() != kinds.Int {
			errs.Add(expr.WrapError(fmt.Errorf("cannot index type %s with non-integer type %s", typ, index.Type())))
		}

		expr.Index = index

		return expr, nil
	case *types.Map:
		index, err := c.resolveExpressionTypes(expr.Index, typ.Key())
		if err != nil {
			errs.Add(err)
		}

		expr.Index = index

		return expr, nil
	default:
		return expr, expr.WrapError(fmt.Errorf("cannot index assign receiver type: %T", expr.Receiver))
	}
}

func (c *Compiler) resolveExpressionTypes(expr Expression, bound types.Type) (_ Expression, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch expr := expr.(type) {
	case *Literal:
		switch val := expr.value.(type) {
		case air.Int:
			if types.IsUnspecified(bound) {
				if types.IsUnspecified(expr.Type()) {
					return &Literal{
						value: expr.value,
						typ:   TypeInt,

						Position: expr.Position,
					}, nil
				} else {
					return expr, nil
				}
			}

			switch bound.Kind() {
			case kinds.Float:
				if types.IsUnspecified(bound) {
					bound = TypeFloat
				}

				expr.typ = bound
				return &Literal{
					value: air.Float(expr.value.(air.Int)),
					typ:   bound,

					Position: expr.Position,
				}, nil
			case kinds.Int:
				if types.IsUnspecified(bound) {
					bound = TypeInt
				}

				return &Literal{
					value: expr.value,
					typ:   bound,

					Position: expr.Position,
				}, nil
			case kinds.Interface:
				err := c.checkInterfaceTypeCoercion(expr, bound)
				if err != nil {
					errs.Add(err)
				}

				return &Literal{
					value: expr.value,
					typ:   TypeInt,

					Position: expr.Position,
				}, nil
			default:
				return expr, expr.WrapError(fmt.Errorf("cannot coerce int literal into %s", bound))
			}
		case air.Float:
			if bound == nil {
				if types.IsUnspecified(expr.Type()) {
					return &Literal{
						value: expr.value,
						typ:   TypeFloat,

						Position: expr.Position,
					}, nil
				} else {
					return expr, nil
				}
			}

			switch bound.Kind() {
			case kinds.Float:
				if types.IsUnspecified(bound) {
					bound = TypeFloat
				}

				expr.typ = bound
				return &Literal{
					value: expr.value,
					typ:   bound,

					Position: expr.Position,
				}, nil
			case kinds.Int:
				if types.IsUnspecified(bound) {
					bound = TypeInt
				}

				return &Literal{
					value: air.Int(expr.value.(air.Float)),
					typ:   bound,

					Position: expr.Position,
				}, nil
			case kinds.Interface:
				err := c.checkInterfaceTypeCoercion(expr, bound)
				if err != nil {
					errs.Add(err)
				}

				return &Literal{
					value: expr.value,
					typ:   TypeFloat,

					Position: expr.Position,
				}, nil
			default:
				return expr, expr.WrapError(fmt.Errorf("cannot coerce float literal into %s", bound))
			}
		case air.String:
			if bound == nil {
				if types.IsUnspecified(expr.Type()) {
					return &Literal{
						value: expr.value,
						typ:   TypeString,

						Position: expr.Position,
					}, nil
				} else {
					return expr, nil
				}
			}

			switch bound.Kind() {
			case kinds.String:
				if types.IsUnspecified(bound) {
					bound = TypeString
				}

				expr.typ = bound
				return &Literal{
					value: expr.value,
					typ:   bound,

					Position: expr.Position,
				}, nil
			case kinds.Interface:
				err := c.checkInterfaceTypeCoercion(expr, bound)
				if err != nil {
					errs.Add(err)
				}

				return &Literal{
					value: expr.value,
					typ:   TypeString,

					Position: expr.Position,
				}, nil
			default:
				return expr, expr.WrapError(fmt.Errorf("cannot coerce string literal into %s", bound))
			}
		case air.Bool:
			if bound == nil {
				if types.IsUnspecified(expr.Type()) {
					return &Literal{
						value: expr.value,
						typ:   TypeBool,

						Position: expr.Position,
					}, nil
				} else {
					return expr, nil
				}
			}

			switch bound.Kind() {
			case kinds.Bool:
				if types.IsUnspecified(bound) {
					bound = TypeBool
				}
				expr.typ = bound
				return &Literal{
					value: expr.value,
					typ:   bound,

					Position: expr.Position,
				}, nil
			case kinds.Interface:
				err := c.checkInterfaceTypeCoercion(expr, bound)
				if err != nil {
					errs.Add(err)
				}

				return &Literal{
					value: expr.value,
					typ:   TypeBool,

					Position: expr.Position,
				}, nil
			default:
				return expr, expr.WrapError(fmt.Errorf("cannot coerce bool literal into %s", bound))
			}
		default:
			return expr, expr.WrapError(fmt.Errorf("unhandled literal type %T", val))
		}
	case *TypeExpression:
		if expr.typ.Kind() == kinds.Unknown {
			errs.Add(fmt.Errorf("type expression %s is unknown", expr.typ))
		}

		return expr, nil
	case *SymbolReferenceExpression:
		sym := expr.Dereference()

		switch sym := sym.(type) {
		case nil:
			return expr, expr.WrapError(fmt.Errorf("undefined name %s", expr.Name()))
		case *Variable:
			return expr, nil
		case types.Type:
			if sym == TypeNil {
				if bound == nil {
					bound = TypeNil
				}

				return &NilExpression{
					typ: bound,

					Position: expr.Position,
				}, nil
			}

			return &TypeExpression{
				typ: sym,

				Position: expr.Position,
			}, nil
		case *Function:
			return expr, nil
		case *ExternFunction:
			return expr, nil
		case *Constant:
			val, err := sym.Evaluate()
			if err != nil {
				errs.Add(expr.WrapError(err))
				return expr, err
			}

			return NewLiteral(val), nil
		case *BuiltinSymbol:
			return expr, nil
		default:
			return expr, expr.WrapError(fmt.Errorf("unhandled symbol type %T", sym))
		}
	case *ParenthesizedExpression:
		subExpr, err := c.resolveExpressionTypes(expr.Expression, bound)
		if err != nil {
			errs.Add(err)
		}

		expr.Expression = subExpr

		return expr, nil
	case *BinaryExpression:
		left, err := c.resolveExpressionTypes(expr.Left, expr.Right.Type())
		if err != nil {
			errs.Add(err)
		}

		expr.Left = left

		right, err := c.resolveExpressionTypes(expr.Right, expr.Left.Type())
		if err != nil {
			errs.Add(err)
		}

		expr.Right = right

		_, err = air.ValidateBinaryExpression(expr.Left.Type(), expr.Operator, expr.Right.Type())
		if err != nil {
			errs.Add(expr.WrapError(err))
		}

		return expr, nil
	case *UnaryExpression:
		switch expr.Operator {
		case operators.Dereference:
			bound = types.NewPointer(bound)
		case operators.Address:
			if ptr, ok := bound.(*types.Pointer); ok {
				bound = ptr.Pointee()
			}
		}

		subExpr, err := c.resolveExpressionTypes(expr.Expression, bound)
		if err != nil {
			errs.Add(err)
		}

		switch subType := subExpr.Type().(type) {
		case *types.TypeType:
			if expr.Operator != operators.Dereference {
				errs.Add(expr.WrapError(fmt.Errorf("invalid operator %s on type expression", expr.Operator)))
			}

			return &TypeExpression{
				typ: types.NewPointer(subType.Type),

				Position: expr.Position,
			}, nil
		default:
			expr.Expression = subExpr

			_, err = air.ValidateUnaryExpression(expr.Expression.Type(), expr.Operator)
			if err != nil {
				errs.Add(expr.WrapError(err))
			}

			return expr, nil
		}
	case *CallExpression:
		fExpr, err := c.resolveCallExpressionReceiverTypes(expr.Function, nil)
		if err != nil {
			errs.Add(err)
		}

		expr.Function = fExpr

		switch ftype := types.Resolve(expr.Function.Type()).(type) {
		case *types.Function:
			var concreteParams []types.Type
			var variadicParam *types.Variadic
			for _, param := range ftype.Parameters {
				if varType, ok := param.(*types.Variadic); ok {
					variadicParam = varType
				} else {
					concreteParams = append(concreteParams, param)
				}
			}

			if variadicParam == nil && len(ftype.Parameters) != len(concreteParams) {
				errs.Add(expr.WrapError(fmt.Errorf("function call expects %d parameters, got %d", len(ftype.Parameters), len(expr.Args))))
			} else if len(expr.Args) < len(concreteParams) {
				errs.Add(expr.WrapError(fmt.Errorf("function call expects at least %d parameters, got %d", len(ftype.Parameters), len(expr.Args))))
			}

			for i := range min(len(expr.Args), len(concreteParams)) {
				arg, err := c.resolveExpressionTypes(expr.Args[i], ftype.Parameters[i])
				if err != nil {
					errs.Add(err)
				}

				if !types.IsAssignableTo(arg.Type(), ftype.Parameters[i]) {
					errs.Add(arg.WrapError(fmt.Errorf("cannot use type %s as type %s in argument %d in function call", arg.Type(), ftype.Parameters[i], i)))
				}

				if ftype.Parameters[i].Kind() == kinds.Interface && arg.Type().Kind() != kinds.Interface {
					expr.Args[i] = &InterfaceCoercionExpression{
						Interface:  types.Resolve(ftype.Parameters[i]).(*types.Interface),
						Expression: arg,
					}
				} else {
					expr.Args[i] = arg
				}
			}

			if variadicParam != nil {
				if len(expr.Args) > len(concreteParams) && expr.Args[len(concreteParams)].Type().Kind() == kinds.Variadic {
					arg, err := c.resolveExpressionTypes(expr.Args[len(concreteParams)], variadicParam)
					if err != nil {
						errs.Add(err)
					}

					if !types.IsAssignableTo(arg.Type().(*types.Variadic).AsSlice(), variadicParam.AsSlice()) {
						errs.Add(arg.WrapError(fmt.Errorf("cannot use type %s as type %s in variadic spread argument in function call", arg.Type().(*types.Variadic).AsSlice(), variadicParam.AsSlice())))
					}

					expr.Args[len(concreteParams)] = arg
				} else {
					for i := len(concreteParams); i < len(expr.Args); i++ {
						if expr.Args[i].Type().Kind() == kinds.Variadic {
							errs.Add(expr.Args[i].WrapError(fmt.Errorf("cannot use variadic spread argument %s as invidiual variadic argument %d in function call", expr.Args[i], i)))
						}

						arg, err := c.resolveExpressionTypes(expr.Args[i], variadicParam.Elem())
						if err != nil {
							errs.Add(err)
						}

						if !types.IsAssignableTo(arg.Type(), variadicParam.Elem()) {
							errs.Add(arg.WrapError(fmt.Errorf("cannot use type %s as type %s in argument %d in function call", arg.Type(), variadicParam.Elem(), i)))
						}

						if variadicParam.Elem().Kind() == kinds.Interface && arg.Type().Kind() != kinds.Interface {
							expr.Args[i] = &InterfaceCoercionExpression{
								Interface:  types.Resolve(variadicParam.Elem()).(*types.Interface),
								Expression: arg,
							}
						} else {
							expr.Args[i] = arg
						}
					}
				}
			}

			return expr, nil
		case *types.TypeType:
			if len(expr.Args) != 1 {
				errs.Add(expr.WrapError(fmt.Errorf("type conversion expects 1 parameter, got %d", len(expr.Args))))
			}

			switch ftype.Type.Kind() {
			case kinds.Interface:
				arg, err := c.resolveExpressionTypes(expr.Args[0], nil)
				if err != nil {
					errs.Add(err)
				}

				return &InterfaceCoercionExpression{
					Interface:  types.Resolve(ftype.Type).(*types.Interface),
					Expression: arg,
					Position:   expr.Position,
				}, nil
			default:
				arg, err := c.resolveExpressionTypes(expr.Args[0], types.ConversionBound(ftype.Type))
				if err != nil {
					errs.Add(err)
				}

				if !types.IsConvertibleTo(arg.Type(), ftype.Type) {
					errs.Add(arg.WrapError(fmt.Errorf("cannot convert type %v to %v", arg.Type(), ftype.Type)))
				}

				expr.Args[0] = arg

				return expr, nil
			}
		case *BuiltinType:
			return ftype.symbol.CallExpression(c, expr)
		default:
			return expr, expr.WrapError(fmt.Errorf("cannot call non-function type %v", fExpr.Type()))
		}
	case *DotExpression:
		receiver, err := c.resolveExpressionTypes(expr.Receiver, nil)
		if err != nil {
			errs.Add(err)
		}
		expr.Receiver = receiver

		return c.resolveDotExpressionReceiverTypes(expr, bound)
	case *IndexExpression:
		receiver, err := c.resolveExpressionTypes(expr.Receiver, nil)
		if err != nil {
			errs.Add(err)
		}
		expr.Receiver = receiver

		return c.resolveIndexExpressionReceiverTypes(expr, expr.Receiver.Type(), bound)
	case *TupleExpression:
		tupleBound, ok := types.Resolve(bound).(*types.Tuple)
		if !ok {
			if !types.IsUnspecified(bound) {
				return expr, expr.WrapError(fmt.Errorf("cannot use tuple literal as %s", bound))
			} else {
				elemBounds := make([]types.Type, len(expr.Elems))
				for i := range expr.Elems {
					elemBounds[i] = types.Unknown
				}
				tupleBound = types.NewTuple(elemBounds...)
			}
		}

		if len(expr.Elems) != len(tupleBound.Elems()) {
			return expr, expr.WrapError(fmt.Errorf("tuple literal expects %d elements, got %d", len(tupleBound.Elems()), len(expr.Elems)))
		}

		for i, elem := range expr.Elems {
			var elemBound types.Type = types.Unknown
			if tupleBound != nil && i < len(tupleBound.Elems()) {
				elemBound = tupleBound.Elems()[i]
			}

			elem, err := c.resolveExpressionTypes(elem, elemBound)
			if err != nil {
				errs.Add(err)
			}

			if !types.IsAssignableTo(elem.Type(), elemBound) {
				errs.Add(elem.WrapError(fmt.Errorf("cannot use element of type %s at index %d in tuple literal as %s", elem.Type(), i, elemBound)))
			}

			if elemBound.Kind() == kinds.Interface && elem.Type().Kind() != kinds.Interface {
				expr.Elems[i] = &InterfaceCoercionExpression{
					Interface:  types.Resolve(elemBound).(*types.Interface),
					Expression: elem,
				}
			} else {
				expr.Elems[i] = elem
			}
		}

		return expr, nil
	case *TypeLiteralExpression:
		exprType, err := c.resolveTypes(expr.Position, expr.Type())
		if err != nil {
			errs.Add(err)
		}

		expr.typ = exprType

		var elemBound types.Type
		if arrayBound, ok := types.Resolve(bound).(*types.Array); ok {
			elemBound = arrayBound.Elem()
		} else if arrayExprType, ok := types.Resolve(exprType).(*types.Array); ok {
			elemBound = arrayExprType.Elem()
		} else {
			elemBound = types.Unknown
		}

		for i, elem := range expr.Elems {
			elem, err := c.resolveExpressionTypes(elem, elemBound)
			if err != nil {
				errs.Add(err)
			}

			if !types.IsAssignableTo(elem.Type(), elemBound) {
				errs.Add(elem.WrapError(fmt.Errorf("cannot use element of type %s at index %d in array literal as %s", elem.Type(), i, elemBound)))
			}

			if elemBound.Kind() == kinds.Interface && elem.Type().Kind() != kinds.Interface {
				elem = &InterfaceCoercionExpression{
					Interface:  types.Resolve(elemBound).(*types.Interface),
					Expression: elem,
				}
			}

			expr.Elems[i] = elem
		}

		return expr, nil
	case *BuiltinExpression:
		err := expr.Impl.TypeCheck(c, expr.Position, expr.Args)
		if err != nil {
			errs.Add(err)
		}

		return expr, nil
	case *SpreadExpression:
		var subBound types.Type
		if bound != nil {
			if bound.Kind() == kinds.Variadic {
				subBound = bound.(*types.Variadic).AsSlice()
			} else {
				errs.Add(expr.WrapError(fmt.Errorf("cannot spread into non-variadic type %s", bound)))
			}
		}

		subExpr, err := c.resolveExpressionTypes(expr.Expr, subBound)
		if err != nil {
			errs.Add(err)
		}

		if !types.IsAssignableTo(subExpr.Type(), subBound) {
			errs.Add(expr.WrapError(fmt.Errorf("cannot spread non-slice type %s", subExpr.Type())))
		}

		expr.Expr = subExpr

		return expr, nil
	case *ErrorReturnExpression:
		switch expr.Function.Return().Kind() {
		case kinds.Interface:
			if !TypeErrorInterface.ImplementedBy(expr.Function.Return()) {
				errs.Add(expr.WrapError(fmt.Errorf("error return expression expects function with error compatible return type, got %s", expr.Function.Return())))
			}
		case kinds.Tuple:
			tupleTyp := types.Resolve(expr.Function.Return()).(*types.Tuple)
			if len(tupleTyp.Elems()) < 1 {
				errs.Add(expr.WrapError(fmt.Errorf("error return expression expects function with error compatible return type, got %s", expr.Function.Return())))
			} else if tupleTyp.Elems()[len(tupleTyp.Elems())-1].Kind() != kinds.Interface || !TypeErrorInterface.ImplementedBy(tupleTyp.Elems()[len(tupleTyp.Elems())-1]) {
				errs.Add(expr.WrapError(fmt.Errorf("error return expression expects error-compatible interface type as last element, got %s", tupleTyp.Elems()[1])))
			}
		default:
			errs.Add(expr.WrapError(fmt.Errorf("error return expression expects function with error compatible return type, got %s", expr.Function.Return())))
		}

		subExpr, err := c.resolveExpressionTypes(expr.Expr, nil)
		if err != nil {
			return expr, err
		}

		expr.Expr = subExpr

		switch subExpr.Type().Kind() {
		case kinds.Interface:
			if !TypeErrorInterface.ImplementedBy(subExpr.Type()) {
				errs.Add(expr.WrapError(fmt.Errorf("error return expression expects error type, got %s", subExpr.Type())))
			}
		case kinds.Tuple:
			tupleType, ok := subExpr.Type().(*types.Tuple)
			if !ok {
				errs.Add(expr.WrapError(fmt.Errorf("error return expression expects tuple type, got %s", subExpr.Type())))
			}

			if tupleType.Elems()[len(tupleType.Elems())-1].Kind() != kinds.Interface || !TypeErrorInterface.ImplementedBy(tupleType.Elems()[len(tupleType.Elems())-1]) {
				errs.Add(expr.WrapError(fmt.Errorf("error return expression expects error-compatible interface type as last element, got %s", tupleType.Elems()[1])))
			}
		default:
			errs.Add(expr.WrapError(fmt.Errorf("error return expression expects error type or tuple ending in error type, got %s", subExpr.Type())))
		}

		return expr, nil
	case *ErrorHandlerExpression:
		subExpr, err := c.resolveExpressionTypes(expr.Expr, nil)
		if err != nil {
			return expr, err
		}

		expr.Expr = subExpr

		handlerExpr, err := c.resolveExpressionTypes(expr.Handler, nil)
		if err != nil {
			return expr, err
		}

		if !types.IsAssignableTo(handlerExpr.Type(), errorHandlerFunctionType) {
			errs.Add(expr.WrapError(fmt.Errorf("error handler expression expects value of type %s, got %s", errorHandlerFunctionType, handlerExpr.Type())))
		}

		expr.Handler = handlerExpr

		return expr, nil
	case *MethodExpression:
		return expr, nil
	case *InterfaceCoercionExpression:
		return expr, nil
	default:
		return expr, expr.WrapError(fmt.Errorf("unhandled expression in type checker %T: %s", expr, expr))
	}
}

func (c *Compiler) resolveCallExpressionReceiverTypes(expr Expression, bound types.Type) (_ Expression, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch expr := expr.(type) {
	case *DotExpression:
		dotExpr, err := c.resolveDotExpressionReceiverTypes(expr, bound)
		if err != nil {
			errs.Add(err)
		}

		// optimization to not allocate a closure for most method calls
		if boundMethod, ok := dotExpr.(*BoundMethodExpression); ok {
			return &MethodExpression{
				Receiver: boundMethod.Receiver,
				Method:   boundMethod.Method,

				Position: expr.Position,
			}, nil
		}

		return dotExpr, nil
	case *ParenthesizedExpression:
		expr.Expression, err = c.resolveCallExpressionReceiverTypes(expr.Expression, bound)
		return expr, err
	default:
		return c.resolveExpressionTypes(expr, bound)
	}
}

func (c *Compiler) resolveDotExpressionReceiverTypes(expr *DotExpression, bound types.Type) (_ Expression, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	var isNumeric bool
	numericKey, err := strconv.Atoi(expr.Key)
	if err == nil {
		isNumeric = true
	}

	expr.Receiver, err = c.resolveExpressionTypes(expr.Receiver, nil)
	if err != nil {
		errs.Add(err)
	}

	var hasMethods bool
	if typ, ok := types.Dereference(expr.Receiver.Type()).(*types.Derived); ok && !isNumeric {
		hasMethods = true

		methods := typ.Methods(false)
		if methods.Has(expr.Key) {
			method, _ := typ.Methods(false).Get(expr.Key)
			targetReceiverType := method.Receiver
			receiver := expr.Receiver

			for {
				_, ok := receiver.Type().(*types.Pointer)
				if !ok || types.Equal(receiver.Type(), targetReceiverType) {
					break
				}

				receiver = &UnaryExpression{
					Expression: receiver,
					Operator:   operators.Dereference,
				}
			}

			if !types.Equal(receiver.Type(), targetReceiverType) {
				errs.Add(expr.WrapError(fmt.Errorf("cannot call method %q on type %s", expr.Key, typ)))
			}

			return &BoundMethodExpression{
				Receiver: receiver,
				Method:   method,

				Position: expr.Position,
			}, nil
		} else if typ.Methods(true).Has(expr.Key) {
			method, _ := typ.Methods(true).Get(expr.Key)
			targetReceiverType := method.Receiver
			receiver := expr.Receiver
			receiver = &UnaryExpression{
				Expression: receiver,
				Operator:   operators.Address,
			}

			if !types.Equal(receiver.Type(), targetReceiverType) {
				errs.Add(expr.WrapError(fmt.Errorf("cannot call method %q on type %s", expr.Key, typ)))
			}

			return &BoundMethodExpression{
				Receiver: receiver,
				Method:   method,

				Position: expr.Position,
			}, nil
		}
	} else if typ, ok := types.Dereference(expr.Receiver.Type()).(*types.Pointer); ok {
		if typ, ok := types.Dereference(typ.Pointee()).(*types.Derived); ok && typ.Methods(true).Has(expr.Key) {
			method, _ := typ.Methods(true).Get(expr.Key)
			targetReceiverType := method.Receiver
			receiver := expr.Receiver

			if !types.Equal(receiver.Type(), targetReceiverType) {
				errs.Add(expr.WrapError(fmt.Errorf("cannot call method %q on type %s", expr.Key, typ)))
			}

			return &BoundMethodExpression{
				Receiver: receiver,
				Method:   method,

				Position: expr.Position,
			}, nil
		}
	}

	switch typ := types.Resolve(expr.Receiver.Type()).(type) {
	case *types.Tuple:
		if isNumeric {
			if !types.IsAssignableTo(typ.Elems()[numericKey], bound) {
				errs.Add(expr.WrapError(fmt.Errorf("cannot use tuple element of type %v at index %d as type %v", typ.Elems()[numericKey], numericKey, bound)))
			}

			if numericKey >= len(typ.Elems()) {
				errs.Add(fmt.Errorf("tuple index %d out bounds on tuple with %d elements", numericKey, len(typ.Elems())))
			}
		} else {
			if hasMethods {
				errs.Add(expr.WrapError(fmt.Errorf("type %s has no such method %s", expr.Type(), expr.Key)))
			} else {
				errs.Add(expr.WrapError(fmt.Errorf("cannot index tuple type %s with %q", expr.Type(), expr.Key)))
			}
		}

		return expr, nil
	case *types.Struct:
		if !typ.HasField(expr.Key) {
			if hasMethods {
				errs.Add(expr.WrapError(fmt.Errorf("type %s no such field or method %q", expr.Type(), expr.Key)))
			} else {
				errs.Add(expr.WrapError(fmt.Errorf("type %s has no such field %q", expr.Type(), expr.Key)))
			}
		}

		return expr, nil
	case *types.Pointer:
		switch typ := types.Resolve(typ.Pointee()).(type) {
		case *types.Tuple:
			if isNumeric {
				if !types.IsAssignableTo(typ.Elems()[numericKey], bound) {
					errs.Add(expr.WrapError(fmt.Errorf("cannot use tuple element of type %v at index %d as type %v", typ.Elems()[numericKey], numericKey, bound)))
				}

				if numericKey >= len(typ.Elems()) {
					errs.Add(fmt.Errorf("tuple index %d out bounds on tuple with %d elements", numericKey, len(typ.Elems())))
				}
			} else {
				if hasMethods {
					errs.Add(expr.WrapError(fmt.Errorf("type %s has no such method %s", expr.Type(), expr.Key)))
				} else {
					errs.Add(expr.WrapError(fmt.Errorf("cannot index tuple type %s with %q", expr.Type(), expr.Key)))
				}
			}

			return expr, nil
		case *types.Struct:
			if !typ.HasField(expr.Key) {
				if hasMethods {
					errs.Add(expr.WrapError(fmt.Errorf("type %s no such field or method %q", expr.Type(), expr.Key)))
				} else {
					errs.Add(expr.WrapError(fmt.Errorf("type %s has no such field %q", expr.Type(), expr.Key)))
				}
			}

			return expr, nil
		default:
			return expr, expr.WrapError(fmt.Errorf("cannot dot index receiver type %s", expr.Receiver.Type()))
		}
	case *types.TypeType:
		_, ok := types.Methods(typ.Type).Get(expr.Key)
		if !ok {
			return expr, expr.WrapError(fmt.Errorf("type %s has no method %s", typ.Type, expr.Key))
		}

		return expr, nil
	case *types.Interface:
		method, ok := typ.Methods().Get(expr.Key)
		if !ok {
			return expr, expr.WrapError(fmt.Errorf("type %s has no method %s", typ, expr.Key))
		}

		return &BoundMethodExpression{
			Receiver: expr.Receiver,
			Method:   method,

			Position: expr.Position,
		}, nil
	default:
		return expr, expr.WrapError(fmt.Errorf("type %s has no field or method %s", typ, expr.Key))
	}
}

func (c *Compiler) resolveIndexExpressionReceiverTypes(expr *IndexExpression, typ types.Type, bound types.Type) (_ Expression, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch typ := types.Resolve(typ).(type) {
	case *types.Array:
		index, err := c.resolveExpressionTypes(expr.Index, nil)
		if err != nil {
			errs.Add(err)
		}

		if index.Type().Kind() != kinds.Int {
			errs.Add(expr.WrapError(fmt.Errorf("cannot index type %s with non-integer type %s", typ, index.Type())))
		}

		expr.Index = index

		return expr, nil
	case *types.Slice:
		index, err := c.resolveExpressionTypes(expr.Index, nil)
		if err != nil {
			errs.Add(err)
		}

		if index.Type().Kind() != kinds.Int {
			errs.Add(expr.WrapError(fmt.Errorf("cannot index type %s with non-integer type %s", typ, index.Type())))
		}

		expr.Index = index

		return expr, nil
	case *types.Map:
		index, err := c.resolveExpressionTypes(expr.Index, typ.Key())
		if err != nil {
			errs.Add(err)
		}

		expr.Index = index

		return expr, nil
	default:
		return expr, expr.WrapError(fmt.Errorf("cannot index receiver type %s", typ))
	}
}

func (c *Compiler) checkInterfaceTypeCoercion(expr Expression, bound types.Type) (err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	ifaceType, ok := types.Resolve(bound).(*types.Interface)
	if !ok {
		errs.Add(expr.WrapError(fmt.Errorf("cannot resolve interface %v", bound)))
		return errs
	}

	if !ifaceType.ImplementedBy(expr.Type()) {
		// TODO: explanation of missing methods
		errs.Add(expr.WrapError(fmt.Errorf("cannot use value of type %v as interface type %v", expr.Type(), bound)))
	}

	return nil
}
