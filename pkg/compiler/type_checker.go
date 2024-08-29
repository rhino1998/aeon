package compiler

import (
	"fmt"
	"strconv"

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

		typ.underlying = newTyp
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

	for _, typ := range pkg.DerivedTypes() {
		for _, m := range typ.MethodFunctions() {
			err := c.resolveFunctionTypes(m)
			if err != nil {
				errs.Add(err)
			}
		}

		for _, m := range typ.PtrMethodFunctions() {
			err := c.resolveFunctionTypes(m)
			if err != nil {
				errs.Add(err)
			}
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

	if f.Receiver().Type().Kind() == KindUnknown {
		errs.Add(f.WrapError(fmt.Errorf("function %q has unknown receiver %q of type %s", f.Name(), f.Receiver().Name(), f.Receiver().Type())))
	}

	for _, param := range f.Parameters() {
		param.typ, err = c.resolveTypes(f.Position, param.Type())
		if err != nil {
			errs.Add(err)
		}

		if param.Type().Kind() == KindUnknown {
			errs.Add(f.WrapError(fmt.Errorf("function %q has unknown parameter %q of type %s", f.Name(), param.Name(), param.Type())))
		}
	}

	f.ret, err = c.resolveTypes(f.Position, f.Return())
	if err != nil {
		errs.Add(err)
	}

	if f.Return().Kind() == KindUnknown {
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

func (c *Compiler) resolveTypes(pos parser.Position, typ Type) (_ Type, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	if !IsTypeResolvable(typ) {
		errs.Add(fmt.Errorf("type %s is unknown", typ))
	}

	switch typ := typ.(type) {
	case *ArrayType:
		err := typ.computeAndValidateLength(nil)
		if err != nil {
			errs.Add(pos.WrapError(err))
		}

		return typ, nil
	case TypeKind:
		switch typ.Kind() {
		case KindInt:
			return TypeInt, nil
		case KindFloat:
			return TypeFloat, nil
		case KindString:
			return TypeString, nil
		case KindBool:
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
		typ := dereferenceType(g.Type())

		g.typ, err = c.resolveTypes(g.Position, typ)
		if err != nil {
			return err
		}

		if !IsTypeResolvable(typ) {
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
			typ := dereferenceType(stmt.Type)

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

			if expr.Type() == TypeVoid {
				errs.Add(expr.WrapError(fmt.Errorf("cannot declare variable of type void")))
			}

			if expr.Type().Kind() == KindType {
				errs.Add(expr.WrapError(fmt.Errorf("cannot use type as a value")))
			}

			if !IsAssignableTo(expr.Type(), stmt.Type) {
				errs.Add(stmt.WrapError(fmt.Errorf("cannot assign type %v to variable %q of type %v", expr.Type(), stmt.Variable.Name(), stmt.Type)))
			}

			if stmt.Type.Kind() == KindInterface && expr.Type().Kind() != KindInterface {
				stmt.Expression = &InterfaceTypeCoercionExpression{
					Interface:  resolveType(stmt.Type).(*InterfaceType),
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

		if expr.Type() == TypeVoid {
			errs.Add(expr.WrapError(fmt.Errorf("cannot declare variable of type void")))
		}

		if expr.Type().Kind() == KindType {
			errs.Add(expr.WrapError(fmt.Errorf("cannot use type as a value")))
		}

		stmt.Expression = expr

		if len(stmt.Variables) > 1 {
			tupleType, ok := resolveType(expr.Type()).(*TupleType)
			if !ok {
				errs.Add(stmt.WrapError(fmt.Errorf("cannot destructure non-tuple type %s", expr.Type())))
			} else {
				for i := range stmt.Variables {
					var varType Type = UnknownType
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
		left, err := c.resolveLHSTypes(stmt.Left)
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

		_, err = validateBinaryExpression(left.Type(), op, right.Type())
		if err != nil {
			errs.Add(stmt.WrapError(err))
		}

		return nil
	case *AssignmentStatement:
		left, err := c.resolveLHSTypes(stmt.Left)
		if err != nil {
			errs.Add(err)
		}

		stmt.Left = left

		right, err := c.resolveExpressionTypes(stmt.Right, left.Type())
		if err != nil {
			errs.Add(err)
		}

		stmt.Right = right

		if !IsAssignableTo(right.Type(), left.Type()) {
			errs.Add(stmt.WrapError(fmt.Errorf("cannot assign type %v to %q of type %v", right.Type(), left, left.Type())))
		}

		if stmt.Left.Type().Kind() == KindInterface && stmt.Right.Type().Kind() != KindInterface {
			stmt.Right = &InterfaceTypeCoercionExpression{
				Interface:  resolveType(stmt.Left.Type()).(*InterfaceType),
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

		_, err = validatePostfixExpression(expr.Type(), stmt.Operator)
		if err != nil {
			errs.Add(stmt.WrapError(err))
		}

		return nil
	case *IfStatement:
		if stmt.Condition != nil {
			cond, err := c.resolveExpressionTypes(stmt.Condition, TypeKind(KindBool))
			if err != nil {
				errs.Add(err)
			}

			if cond.Type().Kind() != KindBool {
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
			cond, err := c.resolveExpressionTypes(stmt.Condition, TypeKind(KindBool))
			if err != nil {
				errs.Add(err)
			}

			if cond.Type().Kind() != KindBool {
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
		if stmt.Expression == nil && stmt.Function.Return() != TypeVoid {
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

func (c *Compiler) resolveLHSTypes(expr Expression) (_ Expression, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch expr := expr.(type) {
	case *Literal:
		return expr, expr.WrapError(fmt.Errorf("cannot assign to literal"))
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
		case *Constant:
			return expr, expr.WrapError(fmt.Errorf("cannot assign to constant %q", expr.Name()))
		default:
			return expr, expr.WrapError(fmt.Errorf("cannot assign to expression type: %T", expr))
		}
	case *ParenthesizedExpression:
		expr.Expression, err = c.resolveLHSTypes(expr.Expression)
		return expr, err
	case *BinaryExpression:
		return expr, expr.WrapError(fmt.Errorf("cannot assign to binary expression"))
	case *UnaryExpression:
		switch expr.Operator {
		case OperatorDereference:
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
	default:
		return expr, expr.WrapError(fmt.Errorf("cannot assign to expression type: %T", expr))
	}
}

func (c *Compiler) resolveLHSDotExpressionReceiverTypes(expr *DotExpression, typ Type) (_ Expression, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch typ := resolveType(typ).(type) {
	case *TupleType:
		index, err := strconv.Atoi(expr.Key)
		if err != nil {
			errs.Add(expr.WrapError(fmt.Errorf("cannot index tuple with %q", expr.Key)))
		}

		if index >= len(typ.Elems()) {
			errs.Add(expr.WrapError(fmt.Errorf("tuple index %d out bounds on tuple with %d elements", index, len(typ.Elems()))))
		}

		return expr, nil
	case *StructType:
		if !typ.HasField(expr.Key) {
			errs.Add(expr.WrapError(fmt.Errorf("struct does not have field %q", expr.Key)))
		}

		return expr, nil
	case *PointerType:
		return c.resolveLHSDotExpressionReceiverTypes(expr, typ.Pointee())
	default:
		return expr, expr.WrapError(fmt.Errorf("cannot dot index assign receiver type: %s", expr.Receiver.Type()))
	}
}

func (c *Compiler) resolveLHSIndexExpressionReceiverTypes(expr *IndexExpression, typ Type) (_ Expression, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch typ := typ.(type) {
	case *ArrayType:
		index, err := c.resolveExpressionTypes(expr.Index, nil)
		if err != nil {
			errs.Add(err)
		}

		if index.Type().Kind() != KindInt {
			errs.Add(expr.WrapError(fmt.Errorf("cannot index type %s with non-integer type %s", typ, index.Type())))
		}

		expr.Index = index

		return expr, nil
	case *SliceType:
		index, err := c.resolveExpressionTypes(expr.Index, nil)
		if err != nil {
			errs.Add(err)
		}

		if index.Type().Kind() != KindInt {
			errs.Add(expr.WrapError(fmt.Errorf("cannot index type %s with non-integer type %s", typ, index.Type())))
		}

		expr.Index = index

		return expr, nil
	case *MapType:
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

func (c *Compiler) resolveExpressionTypes(expr Expression, bound Type) (_ Expression, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch expr := expr.(type) {
	case *Literal:
		switch val := expr.value.(type) {
		case Int:
			if bound == nil {
				if IsUnspecified(expr.Type()) {
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
			case KindFloat:
				if IsUnspecified(bound) {
					bound = TypeFloat
				}

				expr.typ = bound
				return &Literal{
					value: Float(expr.value.(Int)),
					typ:   bound,

					Position: expr.Position,
				}, nil
			case KindInt:
				if IsUnspecified(bound) {
					bound = TypeInt
				}

				return &Literal{
					value: expr.value,
					typ:   bound,

					Position: expr.Position,
				}, nil
			case KindInterface:
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
		case Float:
			if bound == nil {
				if IsUnspecified(expr.Type()) {
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
			case KindFloat:
				if IsUnspecified(bound) {
					bound = TypeFloat
				}

				expr.typ = bound
				return &Literal{
					value: expr.value,
					typ:   bound,

					Position: expr.Position,
				}, nil
			case KindInt:
				if IsUnspecified(bound) {
					bound = TypeInt
				}

				return &Literal{
					value: Int(expr.value.(Float)),
					typ:   bound,

					Position: expr.Position,
				}, nil
			case KindInterface:
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
		case String:
			if bound == nil {
				if IsUnspecified(expr.Type()) {
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
			case KindString:
				if IsUnspecified(bound) {
					bound = TypeString
				}

				expr.typ = bound
				return &Literal{
					value: expr.value,
					typ:   bound,

					Position: expr.Position,
				}, nil
			case KindInterface:
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
		case Bool:
			if bound == nil {
				if IsUnspecified(expr.Type()) {
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
			case KindBool:
				if IsUnspecified(bound) {
					bound = TypeBool
				}
				expr.typ = bound
				return &Literal{
					value: expr.value,
					typ:   bound,

					Position: expr.Position,
				}, nil
			case KindInterface:
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
		if expr.typ.Kind() == KindUnknown {
			errs.Add(fmt.Errorf("type expression %s is unknown", expr.typ))
		}

		return expr, nil
	case *SymbolReferenceExpression:
		sym := expr.Dereference()

		switch sym := sym.(type) {
		case nil:
			return expr, expr.WrapError(fmt.Errorf("undefined name %s", expr.Name()))
		case Nil:
			if bound == nil {
				bound = Nil{}
			}
			return &NilExpression{
				typ: bound,

				Position: expr.Position,
			}, nil
		case *Variable:
			return expr, nil
		case Type:
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

		_, err = validateBinaryExpression(expr.Left.Type(), expr.Operator, expr.Right.Type())
		if err != nil {
			errs.Add(expr.WrapError(err))
		}

		return expr, nil
	case *UnaryExpression:
		switch expr.Operator {
		case OperatorDereference:
			bound = NewPointerType(bound)
		case OperatorAddress:
			if ptr, ok := bound.(*PointerType); ok {
				bound = ptr.Pointee()
			}
		}

		subExpr, err := c.resolveExpressionTypes(expr.Expression, bound)
		if err != nil {
			errs.Add(err)
		}

		switch subType := subExpr.Type().(type) {
		case *TypeType:
			if expr.Operator != OperatorDereference {
				errs.Add(expr.WrapError(fmt.Errorf("invalid operator %s on type expression", expr.Operator)))
			}

			return &TypeExpression{
				typ: NewPointerType(subType.Type),

				Position: expr.Position,
			}, nil
		default:
			expr.Expression = subExpr

			_, err = validateUnaryExpression(expr.Expression.Type(), expr.Operator)
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

		switch ftype := resolveType(expr.Function.Type()).(type) {
		case *FunctionType:
			var concreteParams []Type
			var variadicParam *VariadicType
			for _, param := range ftype.Parameters {
				if varType, ok := param.(*VariadicType); ok {
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

				if !IsAssignableTo(arg.Type(), ftype.Parameters[i]) {
					errs.Add(arg.WrapError(fmt.Errorf("cannot use type %s as type %s in argument %d in function call", arg.Type(), ftype.Parameters[i], i)))
				}

				if ftype.Parameters[i].Kind() == KindInterface && arg.Type().Kind() != KindInterface {
					expr.Args[i] = &InterfaceTypeCoercionExpression{
						Interface:  resolveType(ftype.Parameters[i]).(*InterfaceType),
						Expression: arg,
					}
				} else {
					expr.Args[i] = arg
				}
			}

			if variadicParam != nil {
				if len(expr.Args) > len(concreteParams) && expr.Args[len(concreteParams)].Type().Kind() == KindVariadic {
					arg, err := c.resolveExpressionTypes(expr.Args[len(concreteParams)], variadicParam)
					if err != nil {
						errs.Add(err)
					}

					if !IsAssignableTo(arg.Type().(*VariadicType).AsSlice(), variadicParam.AsSlice()) {
						errs.Add(arg.WrapError(fmt.Errorf("cannot use type %s as type %s in variadic spread argument in function call", arg.Type().(*VariadicType).AsSlice(), variadicParam.AsSlice())))
					}

					expr.Args[len(concreteParams)] = arg
				} else {
					for i := len(concreteParams); i < len(expr.Args); i++ {
						if expr.Args[i].Type().Kind() == KindVariadic {
							errs.Add(expr.Args[i].WrapError(fmt.Errorf("cannot use variadic spread argument %s as invidiual variadic argument %d in function call", expr.Args[i], i)))
						}

						arg, err := c.resolveExpressionTypes(expr.Args[i], variadicParam.Elem())
						if err != nil {
							errs.Add(err)
						}

						if !IsAssignableTo(arg.Type(), variadicParam.Elem()) {
							errs.Add(arg.WrapError(fmt.Errorf("cannot use type %s as type %s in argument %d in function call", arg.Type(), variadicParam.Elem(), i)))
						}

						if variadicParam.Elem().Kind() == KindInterface && arg.Type().Kind() != KindInterface {
							expr.Args[i] = &InterfaceTypeCoercionExpression{
								Interface:  resolveType(variadicParam.Elem()).(*InterfaceType),
								Expression: arg,
							}
						} else {
							expr.Args[i] = arg
						}
					}
				}
			}

			return expr, nil
		case *TypeType:
			if len(expr.Args) != 1 {
				errs.Add(expr.WrapError(fmt.Errorf("type conversion expects 1 parameter, got %d", len(expr.Args))))
			}

			switch ftype.Type.Kind() {
			case KindInterface:
				arg, err := c.resolveExpressionTypes(expr.Args[0], nil)
				if err != nil {
					errs.Add(err)
				}

				return &InterfaceTypeCoercionExpression{
					Interface:  resolveType(ftype.Type).(*InterfaceType),
					Expression: arg,
					Position:   expr.Position,
				}, nil
			default:
				arg, err := c.resolveExpressionTypes(expr.Args[0], TypeConversionBound(ftype.Type))
				if err != nil {
					errs.Add(err)
				}

				if !IsConvertibleTo(arg.Type(), ftype.Type) {
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
		tupleBound, ok := resolveType(bound).(*TupleType)
		if !ok {
			return expr, expr.WrapError(fmt.Errorf("cannot use tuple literal as %s", bound))
		}

		if len(expr.Elems) != len(tupleBound.Elems()) {
			return expr, expr.WrapError(fmt.Errorf("tuple literal expects %d elements, got %d", len(tupleBound.Elems()), len(expr.Elems)))
		}

		for i, elem := range expr.Elems {
			var elemBound Type = UnknownType
			if tupleBound != nil && i < len(tupleBound.Elems()) {
				elemBound = tupleBound.Elems()[i]
			}

			elem, err := c.resolveExpressionTypes(elem, elemBound)
			if err != nil {
				errs.Add(err)
			}

			if !IsAssignableTo(elem.Type(), elemBound) {
				errs.Add(elem.WrapError(fmt.Errorf("cannot use element of type %s at index %d in tuple literal as %s", elem.Type(), i, elemBound)))
			}

			if elemBound.Kind() == KindInterface && elem.Type().Kind() != KindInterface {
				expr.Elems[i] = &InterfaceTypeCoercionExpression{
					Interface:  resolveType(elemBound).(*InterfaceType),
					Expression: elem,
				}
			} else {
				expr.Elems[i] = elem
			}
		}

		return expr, nil
	case *TypeLiteralExpression:
		if arrayType, ok := dereferenceType(expr.Type()).(*ArrayType); ok {
			numElems := len(expr.Elems)
			err := arrayType.computeAndValidateLength(&numElems)
			if err != nil {
				errs.Add(expr.WrapError(err))
			}
		}

		exprType, err := c.resolveTypes(expr.Position, expr.Type())
		if err != nil {
			errs.Add(err)
		}

		expr.typ = exprType

		var elemBound Type
		if arrayBound, ok := resolveType(bound).(*ArrayType); ok {
			elemBound = arrayBound.Elem()
		} else if arrayExprType, ok := resolveType(exprType).(*ArrayType); ok {
			elemBound = arrayExprType.Elem()
		} else {
			elemBound = UnknownType
		}

		for i, elem := range expr.Elems {
			elem, err := c.resolveExpressionTypes(elem, elemBound)
			if err != nil {
				errs.Add(err)
			}

			if !IsAssignableTo(elem.Type(), elemBound) {
				errs.Add(elem.WrapError(fmt.Errorf("cannot use element of type %s at index %d in array literal as %s", elem.Type(), i, elemBound)))
			}

			if elemBound.Kind() == KindInterface && elem.Type().Kind() != KindInterface {
				elem = &InterfaceTypeCoercionExpression{
					Interface:  resolveType(elemBound).(*InterfaceType),
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
		var subBound Type
		if bound != nil {
			if bound.Kind() == KindVariadic {
				subBound = bound.(*VariadicType).AsSlice()
			} else {
				errs.Add(expr.WrapError(fmt.Errorf("cannot spread into non-variadic type %s", bound)))
			}
		}

		subExpr, err := c.resolveExpressionTypes(expr.Expr, subBound)
		if err != nil {
			errs.Add(err)
		}

		if !IsAssignableTo(subExpr.Type(), subBound) {
			errs.Add(expr.WrapError(fmt.Errorf("cannot spread non-slice type %s", subExpr.Type())))
		}

		expr.Expr = subExpr

		return expr, nil
	case *ErrorReturnExpression:
		switch expr.Function.Return().Kind() {
		case KindInterface:
			if !TypeErrorInterface.ImplementedBy(expr.Function.Return()) {
				errs.Add(expr.WrapError(fmt.Errorf("error return expression expects function with error compatible return type, got %s", expr.Function.Return())))
			}
		case KindTuple:
			tupleTyp := resolveType(expr.Function.Return()).(*TupleType)
			if len(tupleTyp.Elems()) < 1 {
				errs.Add(expr.WrapError(fmt.Errorf("error return expression expects function with error compatible return type, got %s", expr.Function.Return())))
			} else if tupleTyp.Elems()[len(tupleTyp.Elems())-1].Kind() != KindInterface || !TypeErrorInterface.ImplementedBy(tupleTyp.Elems()[len(tupleTyp.Elems())-1]) {
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
		case KindInterface:
			if !TypeErrorInterface.ImplementedBy(subExpr.Type()) {
				errs.Add(expr.WrapError(fmt.Errorf("error return expression expects error type, got %s", subExpr.Type())))
			}
		case KindTuple:
			tupleType, ok := subExpr.Type().(*TupleType)
			if !ok {
				errs.Add(expr.WrapError(fmt.Errorf("error return expression expects tuple type, got %s", subExpr.Type())))
			}

			if tupleType.Elems()[len(tupleType.Elems())-1].Kind() != KindInterface || !TypeErrorInterface.ImplementedBy(tupleType.Elems()[len(tupleType.Elems())-1]) {
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

		if !IsAssignableTo(handlerExpr.Type(), errorHandlerFunctionType) {
			errs.Add(expr.WrapError(fmt.Errorf("error handler expression expects value of type %s, got %s", errorHandlerFunctionType, handlerExpr.Type())))
		}

		expr.Handler = handlerExpr

		return expr, nil
	default:
		return expr, expr.WrapError(fmt.Errorf("unhandled expression in type checker: %s", expr))
	}
}

func (c *Compiler) resolveCallExpressionReceiverTypes(expr Expression, bound Type) (_ Expression, err error) {
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

func (c *Compiler) resolveDotExpressionReceiverTypes(expr *DotExpression, bound Type) (_ Expression, err error) {
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
	if typ, ok := dereferenceType(expr.Receiver.Type()).(*DerivedType); ok && !isNumeric {
		hasMethods = true

		methods := typ.Methods(false)
		if methods.Has(expr.Key) {
			method, _ := typ.Methods(false).Get(expr.Key)
			targetReceiverType := method.Receiver
			receiver := expr.Receiver

			for {
				_, ok := receiver.Type().(*PointerType)
				if !ok || TypesEqual(receiver.Type(), targetReceiverType) {
					break
				}

				receiver = &UnaryExpression{
					Expression: receiver,
					Operator:   OperatorDereference,
				}
			}

			if !TypesEqual(receiver.Type(), targetReceiverType) {
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
				Operator:   OperatorAddress,
			}

			if !TypesEqual(receiver.Type(), targetReceiverType) {
				errs.Add(expr.WrapError(fmt.Errorf("cannot call method %q on type %s", expr.Key, typ)))
			}

			return &BoundMethodExpression{
				Receiver: receiver,
				Method:   method,

				Position: expr.Position,
			}, nil
		}
	} else if typ, ok := dereferenceType(expr.Receiver.Type()).(*PointerType); ok {
		if typ, ok := dereferenceType(typ.Pointee()).(*DerivedType); ok && typ.Methods(true).Has(expr.Key) {
			method, _ := typ.Methods(true).Get(expr.Key)
			targetReceiverType := method.Receiver
			receiver := expr.Receiver

			if !TypesEqual(receiver.Type(), targetReceiverType) {
				errs.Add(expr.WrapError(fmt.Errorf("cannot call method %q on type %s", expr.Key, typ)))
			}

			return &BoundMethodExpression{
				Receiver: receiver,
				Method:   method,

				Position: expr.Position,
			}, nil
		}
	}

	switch typ := resolveType(expr.Receiver.Type()).(type) {
	case *TupleType:
		if isNumeric {
			if !IsAssignableTo(typ.Elems()[numericKey], bound) {
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
	case *StructType:
		if !typ.HasField(expr.Key) {
			if hasMethods {
				errs.Add(expr.WrapError(fmt.Errorf("type %s no such field or method %q", expr.Type(), expr.Key)))
			} else {
				errs.Add(expr.WrapError(fmt.Errorf("type %s has no such field %q", expr.Type(), expr.Key)))
			}
		}

		return expr, nil
	case *PointerType:
		switch typ := resolveType(typ.Pointee()).(type) {
		case *TupleType:
			if isNumeric {
				if !IsAssignableTo(typ.Elems()[numericKey], bound) {
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
		case *StructType:
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
	case *TypeType:
		_, ok := TypeMethod(typ.Type, expr.Key)
		if !ok {
			return expr, expr.WrapError(fmt.Errorf("type %s has no method %s", typ.Type, expr.Key))
		}

		return expr, nil
	case *InterfaceType:
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

func (c *Compiler) resolveIndexExpressionReceiverTypes(expr *IndexExpression, typ Type, bound Type) (_ Expression, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch typ := resolveType(typ).(type) {
	case *ArrayType:
		index, err := c.resolveExpressionTypes(expr.Index, nil)
		if err != nil {
			errs.Add(err)
		}

		if index.Type().Kind() != KindInt {
			errs.Add(expr.WrapError(fmt.Errorf("cannot index type %s with non-integer type %s", typ, index.Type())))
		}

		expr.Index = index

		return expr, nil
	case *SliceType:
		index, err := c.resolveExpressionTypes(expr.Index, nil)
		if err != nil {
			errs.Add(err)
		}

		if index.Type().Kind() != KindInt {
			errs.Add(expr.WrapError(fmt.Errorf("cannot index type %s with non-integer type %s", typ, index.Type())))
		}

		expr.Index = index

		return expr, nil
	case *MapType:
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

func (c *Compiler) checkInterfaceTypeCoercion(expr Expression, bound Type) (err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	ifaceType, ok := resolveType(bound).(*InterfaceType)
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
