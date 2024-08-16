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
		err := c.resolveStatementTypes(stmt)
		if err != nil {
			errs.Add(err)
		}
	}

	return nil
}

func (c *Compiler) resolveGlobalTypes(pkg *Package, g *Variable) (err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	if g.Type() != nil {
		typ := resolveType(g.Type())

		if kindType, ok := typ.(TypeKind); ok {
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
			typ := resolveType(stmt.Type)

			if kindType, ok := typ.(TypeKind); ok {
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

			if !IsTypeResolvable(typ) {
				errs.Add(stmt.WrapError(fmt.Errorf("type %s is unknown", typ)))
			}

			stmt.Type = typ
			stmt.Variable.SetType(stmt.Type)
		}

		if stmt.Expression != nil {
			expr, err := c.resolveExpressionTypes(stmt.Expression, stmt.Type)
			if err != nil {
				errs.Add(err)
			}

			if expr.Type() == VoidType {
				errs.Add(expr.WrapError(fmt.Errorf("cannot declare variable of type void")))
			}

			if expr.Type().Kind() == KindTypeConversion {
				errs.Add(expr.WrapError(fmt.Errorf("cannot use type conversion as a value")))
			}

			if !IsAssignableTo(expr.Type(), stmt.Type) {
				errs.Add(stmt.WrapError(fmt.Errorf("cannot assign type %v to variable of type %v", expr.Type(), stmt.Type)))
			}

			if stmt.Type.Kind() == KindInterface && expr.Type().Kind() != KindInterface {
				stmt.Expression = &InterfaceTypeCoercionExpression{
					Interface:  BaseType(stmt.Type).(*InterfaceType),
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

		if expr.Type() == VoidType {
			errs.Add(expr.WrapError(fmt.Errorf("cannot declare variable of type void")))
		}

		if expr.Type().Kind() == KindTypeConversion {
			errs.Add(expr.WrapError(fmt.Errorf("cannot use type conversion as a value")))
		}

		stmt.Expression = expr
		stmt.Variable.SetType(expr.Type())

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

		_, err = validateBinaryExpression(left.Type(), stmt.Operator, right.Type())
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
			errs.Add(stmt.WrapError(fmt.Errorf("cannot assign type %v to variable of type %v", right.Type(), left.Type())))
		}

		if stmt.Left.Type().Kind() == KindInterface && stmt.Right.Type().Kind() != KindInterface {
			stmt.Right = &InterfaceTypeCoercionExpression{
				Interface:  BaseType(stmt.Left.Type()).(*InterfaceType),
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

		// TODO: validate by operator

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
		if stmt.Expression != nil && stmt.Function.Return() == VoidType {
			errs.Add(stmt.WrapError(fmt.Errorf("unexpected return value on void function")))
		}

		if stmt.Expression == nil && stmt.Function.Return() != VoidType {
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

	switch typ := BaseType(typ).(type) {
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
			if bound == nil && IsUnspecified(expr.Type()) {
				return &Literal{
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
				return &Literal{
					value: Float(expr.value.(Int)),
					typ:   bound,

					Position: expr.Position,
				}, nil
			case KindInt:
				if IsUnspecified(bound) {
					bound = IntType
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
					typ:   IntType,

					Position: expr.Position,
				}, nil
			default:
				return expr, expr.WrapError(fmt.Errorf("cannot coerce int literal into %s", bound))
			}
		case Float:
			if bound == nil && IsUnspecified(expr.Type()) {
				return &Literal{
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
				return &Literal{
					value: expr.value,
					typ:   bound,

					Position: expr.Position,
				}, nil
			case KindInt:
				if IsUnspecified(bound) {
					bound = IntType
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
					typ:   FloatType,

					Position: expr.Position,
				}, nil
			default:
				return expr, expr.WrapError(fmt.Errorf("cannot coerce float literal into %s", bound))
			}
		case String:
			if bound == nil && IsUnspecified(expr.Type()) {
				return &Literal{
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
					typ:   StringType,

					Position: expr.Position,
				}, nil
			default:
				return expr, expr.WrapError(fmt.Errorf("cannot coerce string literal into %s", bound))
			}
		case Bool:
			if bound == nil && IsUnspecified(expr.Type()) {
				return &Literal{
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
					typ:   BoolType,

					Position: expr.Position,
				}, nil
			default:
				return expr, expr.WrapError(fmt.Errorf("cannot coerce bool literal into %s", bound))
			}
		default:
			return expr, expr.WrapError(fmt.Errorf("unhandled literal type %T", val))
		}
	case *SymbolReferenceExpression:
		sym := expr.Dereference()

		switch sym := sym.(type) {
		case nil:
			return expr, expr.WrapError(fmt.Errorf("undefined name %s", expr.Name()))
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
			}

			return NewLiteral(val), nil
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

		expr.Expression = subExpr

		_, err = validateUnaryExpression(expr.Expression.Type(), expr.Operator)
		if err != nil {
			errs.Add(expr.WrapError(err))
		}

		return expr, nil
	case *CallExpression:
		fExpr, err := c.resolveCallExpressionReceiverTypes(expr.Function, nil)
		if err != nil {
			errs.Add(err)
		}

		expr.Function = fExpr

		switch ftype := BaseType(expr.Function.Type()).(type) {
		case *FunctionType:
			if len(ftype.Parameters) != len(expr.Args) {
				errs.Add(expr.WrapError(fmt.Errorf("function call expects %d parameters, got %d", len(ftype.Parameters), len(expr.Args))))
			}

			for i := range min(len(expr.Args), len(ftype.Parameters)) {
				arg, err := c.resolveExpressionTypes(expr.Args[i], ftype.Parameters[i])
				if err != nil {
					errs.Add(err)
				}

				if !IsAssignableTo(arg.Type(), ftype.Parameters[i]) {
					errs.Add(arg.WrapError(fmt.Errorf("cannot use type %s as type %s in argument %d in function call", arg.Type(), ftype.Parameters[i], i)))
				}

				expr.Args[i] = arg
			}
		case *TypeType:
			if len(expr.Args) != 1 {
				errs.Add(expr.WrapError(fmt.Errorf("type conversion expects 1 parameter, got %d", len(expr.Args))))
			}

			arg, err := c.resolveExpressionTypes(expr.Args[0], TypeKind(ftype.Type.Kind()))
			if err != nil {
				errs.Add(err)
			}

			expr.Args[0] = arg

			if len(expr.Args) > 0 && !IsConvertibleTo(expr.Args[0].Type(), ftype.Type) {
				errs.Add(expr.WrapError(fmt.Errorf("cannot convert type %v to %v", expr.Args[0].Type(), ftype.Type)))
			}

			return expr, nil
		default:
			errs.Add(expr.WrapError(fmt.Errorf("cannot call non-function type %v", fExpr.Type())))
		}

		return expr, nil
	case *DotExpression:
		receiver, err := c.resolveExpressionTypes(expr.Receiver, nil)
		if err != nil {
			errs.Add(err)
		}
		expr.Receiver = receiver

		return c.resolveDotExpressionReceiverTypes(expr, expr.Receiver.Type(), false, bound)
	case *IndexExpression:
		receiver, err := c.resolveExpressionTypes(expr.Receiver, nil)
		if err != nil {
			errs.Add(err)
		}
		expr.Receiver = receiver

		return c.resolveIndexExpressionReceiverTypes(expr, expr.Receiver.Type(), bound)
	case *TupleExpression:
		tupleBound, _ := BaseType(bound).(*TupleType)
		for i, elem := range expr.Elems {
			var elemBound Type
			if tupleBound != nil {
				elemBound = tupleBound.Elems()[i]
			}

			elem, err := c.resolveExpressionTypes(elem, elemBound)
			if err != nil {
				errs.Add(err)
			}

			expr.Elems[i] = elem
		}

		return expr, nil
	case *ArrayExpression:
		arrayBound, ok := BaseType(bound).(*ArrayType)
		var elemBound Type
		if ok {
			elemBound = arrayBound.Elem()
		} else {
			elemBound = expr.ElemType
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
					Interface:  BaseType(elemBound).(*InterfaceType),
					Expression: elem,
				}
			}

			expr.Elems[i] = elem
		}

		return expr, nil
	default:
		return expr, expr.WrapError(fmt.Errorf("unhandled expression in type checker: %T", expr))
	}
}

func (c *Compiler) resolveCallExpressionReceiverTypes(expr Expression, bound Type) (_ Expression, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch expr := expr.(type) {
	case *DotExpression:
		dotExpr, err := c.resolveDotExpressionReceiverTypes(expr, expr.Receiver.Type(), false, bound)
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

func (c *Compiler) resolveDotExpressionReceiverTypes(expr *DotExpression, typ Type, ptr bool, bound Type) (_ Expression, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	var isNumeric bool
	numericKey, err := strconv.Atoi(expr.Key)
	if err == nil {
		isNumeric = true
	}

	var hasMethods bool
	if typ, ok := resolveType(typ).(*DerivedType); ok && !isNumeric {
		hasMethods = true

		methods := typ.Methods(ptr)
		// TODO: auto address of
		if methods.Has(expr.Key) {
			method, _ := typ.Methods(ptr).Get(expr.Key)
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
		} else if !ptr && typ.Methods(true).Has(expr.Key) {
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
				Receiver: expr,
				Method:   method,

				Position: expr.Position,
			}, nil
		}
	}

	switch typ := BaseType(typ).(type) {
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
		return c.resolveDotExpressionReceiverTypes(expr, typ.Pointee(), true, bound)
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
		return expr, expr.WrapError(fmt.Errorf("type %s has no method %s", typ, expr.Key))
	}
}

func (c *Compiler) resolveIndexExpressionReceiverTypes(expr *IndexExpression, typ Type, bound Type) (_ Expression, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch typ := BaseType(typ).(type) {
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

	ifaceType, ok := BaseType(bound).(*InterfaceType)
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
