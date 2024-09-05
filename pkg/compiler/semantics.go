package compiler

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler/air"
	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/operators"
	"github.com/rhino1998/aeon/pkg/compiler/types"
	"github.com/rhino1998/aeon/pkg/parser"
)

func (c *Compiler) compileFile(prog *Program, filename string, entry parser.File) (err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	pkg := prog.AddPackage(string(entry.Package.Name.Str))

	for _, decl := range entry.Declarations {
		switch decl := decl.(type) {
		case parser.FunctionDeclaration:
			pkg.scope.put(SymbolStub(decl.Name.Str))
		case parser.TypeDeclaration:
			pkg.scope.put(SymbolStub(decl.Name.Str))
		case parser.ExternFunctionDeclaration:
			pkg.scope.put(SymbolStub(decl.Name.Str))
		case parser.VarDeclaration:
			pkg.scope.put(SymbolStub(decl.Name.Str))
		case parser.ConstDeclaration:
			pkg.scope.put(SymbolStub(decl.Name.Str))
		}
	}

	for _, decl := range entry.Declarations {
		if _, ok := decl.(parser.TypeDeclaration); !ok {
			continue
		}
		_, err := c.compileDeclaration(pkg, pkg.scope, decl)
		if err != nil {
			errs.Add(err)
		}
	}

	// TODO: sort topologically
	for _, decl := range entry.Declarations {
		if _, ok := decl.(parser.TypeDeclaration); ok {
			continue
		}

		_, err := c.compileDeclaration(pkg, pkg.scope, decl)
		if err != nil {
			errs.Add(err)
		}
	}

	return nil
}

func (c *Compiler) compileDeclaration(p *Package, scope *SymbolScope, decl parser.Declaration) (Symbol, error) {
	switch decl := decl.(type) {
	case parser.FunctionDeclaration:
		return c.compileFunction(p, scope, decl)
	case parser.MethodDeclaration:
		return c.compileMethod(p, scope, decl)
	case parser.TypeDeclaration:
		return c.compileTypeDeclaration(p, scope, decl)
	case parser.ExternFunctionDeclaration:
		return c.compileExternFunctionDeclaration(p, scope, decl)
	case parser.VarDeclaration:
		return c.compileVarDeclaration(p, scope, decl)
	case parser.ConstDeclaration:
		return c.compileConstDeclaration(p, scope, decl)
	case parser.Directive:
		return c.compileDirective(p, scope, decl)
	default:
		return nil, decl.WrapError(fmt.Errorf("unrecognized declaration type: %T", decl))
	}
}

func (c *Compiler) compileDirective(p *Package, scope *SymbolScope, dir parser.Directive) (_ Symbol, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	sym, err := c.compileDeclaration(p, scope, dir.Declaration)
	if err != nil {
		errs.Add(err)
	}

	switch sym := sym.(type) {
	case *Function:
		switch dir.Name.Str {
		case "init":
			if len(dir.Args) != 0 {
				errs.Add(dir.WrapError(fmt.Errorf("expected 0 arguments for directive %q", dir.Name)))
			}

			p.initFuncs = append(p.initFuncs, sym)
		case "update":
			if len(dir.Args) != 0 {
				errs.Add(dir.WrapError(fmt.Errorf("expected 0 arguments for directive %q", dir.Name)))
			}

			p.updateFuncs = append(p.updateFuncs, sym)
		default:
			return nil, dir.WrapError(fmt.Errorf("unknown directive %q", dir.Name))
		}
	case *ExternFunction:
		return nil, dir.WrapError(fmt.Errorf("directive %q is not supported for extern declarations", dir.Name))
	case *types.Derived:
		return nil, dir.WrapError(fmt.Errorf("directive %q is not supported for type declarations", dir.Name))
	case *Constant:
		return nil, dir.WrapError(fmt.Errorf("directive %q is not supported for constant declarations", dir.Name))
	case *Variable:
		return nil, dir.WrapError(fmt.Errorf("directive %q is not supported for variable declarations", dir.Name))
	default:
		return nil, fmt.Errorf("compiler error: unhandled symbol type %T", sym)
	}

	return sym, nil
}

func (c *Compiler) compileExternFunctionDeclaration(p *Package, scope *SymbolScope, decl parser.ExternFunctionDeclaration) (_ *ExternFunction, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	f := &ExternFunction{
		name: string(decl.Name.Str),
	}

	var lastParamType parser.Type
	f.parameters = make([]*Variable, len(decl.Parameters))
	last := true
	for i := len(decl.Parameters) - 1; i >= 0; i-- {
		param := decl.Parameters[i]
		var paramName string
		if param.Name != nil {
			paramName = string(param.Name.Str)
		}

		if param.Type == nil {
			if lastParamType == nil {
				errs.Add(param.WrapError(fmt.Errorf("missing type for parameter %q", param.Name.Str)))
			}

			param.Type = lastParamType
		} else {
			lastParamType = param.Type
		}

		typ, err := c.compileTypeReference(scope, param.Type)
		if err != nil {
			errs.Add(err)
		}

		varType, isVariadic := types.Resolve(typ).(*types.Variadic)
		if isVariadic {
			if !last {
				errs.Add(decl.WrapError(fmt.Errorf("only the last paramater may be variadic")))
			}

			typ = varType.AsSlice()
		}

		variable := NewVariable(paramName, typ, scope)
		variable.variadic = isVariadic
		variable.Position = param.Position

		f.parameters[i] = variable
		last = false
	}

	if decl.Return != nil {
		typ, err := c.compileTypeReference(scope, decl.Return)
		if err != nil {
			errs.Add(err)
		}

		f.ret = typ
	} else {
		f.ret = types.Void
	}

	err = scope.put(f)
	if err != nil {
		errs.Add(decl.WrapError(err))
	}

	return f, nil
}

func (c *Compiler) compileTypeDeclaration(p *Package, scope *SymbolScope, decl parser.TypeDeclaration) (_ *types.Derived, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	underlying, err := c.compileTypeReference(scope, decl.Type)
	if err != nil {
		errs.Add(err)
	}

	t := types.NewDerived(string(decl.Name.Str), p, underlying)
	t.Position = decl.Position

	err = scope.put(t)
	if err != nil {
		errs.Add(err)
	}

	return t, nil
}

func (c *Compiler) compileVarDeclaration(p *Package, scope *SymbolScope, decl parser.VarDeclaration) (_ *Variable, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	var varTyp types.Type

	if decl.Type != nil {
		varTyp, err = c.compileTypeReference(scope, *decl.Type)
		if err != nil {
			errs.Add(err)
		}
	}

	var varExpr Expression

	if decl.Expr != nil {
		varExpr, err = c.compileExpression(scope, *decl.Expr)
		if err != nil {
			errs.Add(err)
		}

		if varTyp == nil {
			varTyp = varExpr.Type()
		}
	}

	if varTyp == nil {
		varTyp = types.Unknown
		errs.Add(decl.WrapError(fmt.Errorf("variable declaration must have a type or an expression")))
	}

	v := NewVariable(string(decl.Name.Str), varTyp, scope)
	v.Position = decl.Position
	v.expr = varExpr

	err = scope.put(v)
	if err != nil {
		errs.Add(err)
	}

	return v, nil
}

func (c *Compiler) compileConstDeclaration(p *Package, scope *SymbolScope, decl parser.ConstDeclaration) (_ *Constant, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	v := &Constant{
		name:  string(decl.Name.Str),
		scope: scope,

		Position: decl.Position,
	}
	if decl.Type != nil {
		typ, err := c.compileTypeReference(scope, *decl.Type)
		if err != nil {
			errs.Add(err)
		}

		v.typ = typ
	}

	if decl.Expr != nil {
		expr, err := c.compileExpression(scope, *decl.Expr)
		if err != nil {
			errs.Add(err)
		}

		v.ConstantExpression = expr.(ConstantExpression)

		if v.typ == nil {
			v.typ = expr.Type()
		}
	}

	if v.typ == nil {
		v.typ = types.Unknown
		errs.Add(decl.WrapError(fmt.Errorf("const declaration must have a type or an expression")))
	}

	err = scope.put(v)
	if err != nil {
		errs.Add(decl.WrapError(err))
	}

	return v, nil
}

func (c *Compiler) compileTypeReference(scope *SymbolScope, typ parser.Type) (_ types.Type, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch typ := typ.(type) {
	case nil:
		return types.Unknown, nil
	case parser.Identifier:
		return types.NewReference(scope, string(typ.Str)), nil
	case parser.PointerType:
		pointee, err := c.compileTypeReference(scope, typ.Pointee)
		if err != nil {
			errs.Add(err)
		}

		return types.NewPointer(pointee), nil
	case parser.SliceType:
		elem, err := c.compileTypeReference(scope, typ.Element)
		if err != nil {
			errs.Add(err)
		}

		return types.NewSlice(elem), nil
	case parser.MapType:
		key, err := c.compileTypeReference(scope, typ.Key)
		if err != nil {
			errs.Add(err)
		}

		val, err := c.compileTypeReference(scope, typ.Key)
		if err != nil {
			errs.Add(err)
		}

		return types.NewMap(key, val), nil
	case parser.TupleType:
		var elems []types.Type
		for _, elem := range typ.Elements {
			elemTyp, err := c.compileTypeReference(scope, elem)
			if err != nil {
				errs.Add(err)
			}

			elems = append(elems, elemTyp)
		}

		return types.NewTuple(elems...), nil
	case parser.ArrayType:
		elem, err := c.compileTypeReference(scope, typ.Element)
		if err != nil {
			errs.Add(err)
		}

		var length int = -1

		var lengthExpr ConstantExpression
		if typ.Length != nil {
			maybeLengthExpr, err := c.compileExpression(scope, typ.Length)
			if err != nil {
				errs.Add(err)
			}
			var ok bool
			lengthExpr, ok = maybeLengthExpr.(ConstantExpression)
			if !ok {
				return nil, typ.Length.WrapError(fmt.Errorf("array length must be a constant expression"))
			}

			lengthValue, err := lengthExpr.Evaluate()
			if err != nil {
				return nil, typ.Length.WrapError(err)
			}

			length = int(lengthValue.Value().Operand.Value.(air.Int))
			if length < 0 {
				return nil, typ.Length.WrapError(fmt.Errorf("array length must be non-negative"))
			}
		}

		return types.NewArray(length, elem), nil
	case parser.StructType:
		t := types.NewStruct()
		for _, field := range typ.Fields {
			fieldType, err := c.compileTypeReference(scope, field.Type)
			if err != nil {
				errs.Add(err)
			}

			t = t.With(string(field.Name.Str), fieldType)
		}

		return t, nil
	case parser.FunctionType:
		var lastParamType parser.Type
		parameters := make([]types.Type, len(typ.Parameters))
		for i := len(typ.Parameters) - 1; i >= 0; i-- {
			param := typ.Parameters[i]
			if param.Type == nil {
				if lastParamType == nil {
					errs.Add(param.WrapError(fmt.Errorf("missing type for parameter %q", param.Name.Str)))
				}

				param.Type = lastParamType
			} else {
				lastParamType = param.Type
			}

			typ, err := c.compileTypeReference(scope, param.Type)
			if err != nil {
				errs.Add(err)
			}

			parameters[i] = typ
		}
		var ret types.Type = types.Void
		if typ.Return != nil {
			ret, err = c.compileTypeReference(scope, typ.Return)
			if err != nil {
				errs.Add(err)
			}
		}

		return &types.Function{
			Receiver:   types.Void,
			Return:     ret,
			Parameters: parameters,
		}, nil
	case parser.InterfaceType:

		t := types.NewInterface()
		for _, field := range typ.Methods {
			var params []types.Type
			for _, param := range field.Parameters {
				paramType, err := c.compileTypeReference(scope, param.Type)
				if err != nil {
					if err != nil {
						errs.Add(err)
					}
				}

				params = append(params, paramType)
			}

			var retType types.Type = types.Void
			if field.Return != nil {
				retType, err = c.compileTypeReference(scope, field.Return)
				if err != nil {
					errs.Add(err)
				}
			}

			t = t.With(string(field.Name.Str), params, retType)
		}

		return t, nil
	case parser.ParenthesizedType:
		inner, err := c.compileTypeReference(scope, typ.Type)
		if err != nil {
			errs.Add(err)
		}

		return inner, nil
	case parser.VariadicType:
		elem, err := c.compileTypeReference(scope, typ.Type)
		if err != nil {
			errs.Add(err)
		}

		return types.NewVariadic(elem), nil
	default:
		return types.Unknown, typ.WrapError(fmt.Errorf("unhandled type reference %q", typ))
	}
}

func (c *Compiler) compileFunction(p *Package, scope *SymbolScope, decl parser.FunctionDeclaration) (_ *Function, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	f := newFunction(string(decl.Name.Str), p)
	f.Position = decl.Position

	f.receiver = NewVariable("", types.Void, scope)

	var lastParamType parser.Type
	f.parameters = make([]*Variable, len(decl.Parameters))
	last := true
	for i := len(decl.Parameters) - 1; i >= 0; i-- {
		param := decl.Parameters[i]
		var paramName string
		if param.Name != nil {
			paramName = string(param.Name.Str)
		}

		if param.Type == nil {
			if lastParamType == nil {
				errs.Add(param.WrapError(fmt.Errorf("missing type for parameter %q", param.Name.Str)))
			}

			param.Type = lastParamType
		} else {
			lastParamType = param.Type
		}

		typ, err := c.compileTypeReference(scope, param.Type)
		if err != nil {
			errs.Add(err)
		}

		varType, isVariadic := types.Resolve(typ).(*types.Variadic)
		if isVariadic {
			if !last {
				return nil, decl.WrapError(fmt.Errorf("only the last paramater may be variadic"))
			}

			typ = varType.AsSlice()
		}

		variable := NewVariable(paramName, typ, scope)
		variable.variadic = isVariadic
		variable.Position = param.Position

		f.parameters[i] = variable
		last = false
	}

	for _, v := range f.parameters {
		if v.Name() == "" {
			continue
		}
		err := f.symbols.put(v)
		if err != nil {
			errs.Add(v.WrapError(err))
		}
	}

	if decl.Return != nil {
		typ, err := c.compileTypeReference(scope, decl.Return)
		if err != nil {
			errs.Add(err)
		}

		f.ret = typ
	} else {
		f.ret = types.Void
	}

	f.symbols.put(f)

	f.body, err = c.compileStatements(f.symbols, decl.Body)
	if err != nil {
		errs.Add(err)
	}

	err = scope.put(f)
	if err != nil {
		errs.Add(err)
	}

	return f, nil
}

func (c *Compiler) compileMethod(p *Package, scope *SymbolScope, decl parser.MethodDeclaration) (_ *Function, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	var recvName string
	if decl.Receiver.Name != nil {
		recvName = string(decl.Receiver.Name.Str)
	}

	recvTyp, err := c.compileTypeReference(scope, decl.Receiver.Type)
	if err != nil {
		errs.Add(err)
	}

	if !types.IsValidMethodReceiverType(recvTyp) {
		errs.Add(decl.WrapError(fmt.Errorf("cannot use %s as a method receiver", recvTyp)))
	}

	var recvDerivedType *types.Derived
	switch recvTyp := types.Dereference(recvTyp).(type) {
	case *types.Derived:
		recvDerivedType = recvTyp
	case *types.Pointer:
		switch recvTyp := types.Dereference(recvTyp.Pointee()).(type) {
		case *types.Derived:
			recvDerivedType = recvTyp
		default:
			errs.Add(decl.WrapError(fmt.Errorf("cannot use %s as a method receiver", recvTyp)))
			return nil, errs
		}
	default:
		errs.Add(decl.WrapError(fmt.Errorf("cannot use %s as a method receiver", recvTyp)))
		return nil, errs
	}

	if recvDerivedType.NamespaceQualifiedName() != p.QualifiedName() {
		errs.Add(decl.WrapError(fmt.Errorf("cannot declare method on receiver type %s which is not defined in this package", recvTyp)))
		return nil, errs
	}

	f := newFunction(string(decl.Name.String()), p)
	f.Position = decl.Position

	recvVar := NewVariable(recvName, recvTyp, scope)
	recvVar.Position = decl.Receiver.Position

	f.receiver = recvVar

	if decl.Receiver.Name != nil {
		f.symbols.put(recvVar)
	}

	var lastParamType parser.Type
	f.parameters = make([]*Variable, len(decl.Parameters))
	last := true
	for i := len(decl.Parameters) - 1; i >= 0; i-- {
		param := decl.Parameters[i]
		var paramName string
		if param.Name != nil {
			paramName = string(param.Name.Str)
		}

		if param.Type == nil {
			if lastParamType == nil {
				errs.Add(param.WrapError(fmt.Errorf("missing type for parameter %q", param.Name.Str)))
			}

			param.Type = lastParamType
		} else {
			lastParamType = param.Type
		}

		typ, err := c.compileTypeReference(scope, param.Type)
		if err != nil {
			errs.Add(err)
		}

		varType, isVariadic := types.Resolve(typ).(*types.Variadic)
		if isVariadic {
			if !last {
				return nil, decl.WrapError(fmt.Errorf("only the last paramater may be variadic"))
			}

			typ = varType.AsSlice()
		}

		variable := NewVariable(paramName, typ, scope)
		variable.variadic = isVariadic
		variable.Position = param.Position

		f.parameters[i] = variable
		last = false
	}

	for _, v := range f.parameters {
		if v.Name() == "" {
			continue
		}
		err := f.symbols.put(v)
		if err != nil {
			errs.Add(v.WrapError(err))
		}
	}

	if decl.Return != nil {
		typ, err := c.compileTypeReference(scope, decl.Return)
		if err != nil {
			errs.Add(err)
		}

		f.ret = typ
	} else {
		f.ret = types.Void
	}

	err = p.scope.put(f)
	if err != nil {
		errs.Add(decl.WrapError(err))
	}

	f.body, err = c.compileStatements(f.symbols, decl.Body)
	if err != nil {
		errs.Add(err)
	}

	if f.Return() != types.Void {
		var last Statement
		var wrapper ErrorWrapper = decl
		if len(f.body) > 0 {
			last = f.body[len(f.body)-1]
			wrapper = last
		}

		if _, ok := last.(*ReturnStatement); !ok {
			errs.Add(wrapper.WrapError(fmt.Errorf("missing return statement at end of non-void function")))
		}
	}

	recvDerivedType.AddMethod(string(decl.Name.Str), f.Type().(*types.Function))

	if _, ok := recvTyp.(*types.Pointer); !ok {
		ptrF := f.withPointerReceiver()
		err = p.scope.put(ptrF)
		if err != nil {
			return nil, decl.WrapError(err)
		}
	}

	return f, nil
}

func (c *Compiler) compileStatement(scope *SymbolScope, stmt parser.Statement) (_ Statement, _ *SymbolScope, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch stmt := stmt.(type) {
	case *parser.VarStatement:
		var expr Expression
		if stmt.Expr != nil {
			expr, err = c.compileExpression(scope, *stmt.Expr)
			if err != nil {
				errs.Add(err)
			}
		}

		var typ types.Type
		if stmt.Type != nil {
			typ, err = c.compileTypeReference(scope, *stmt.Type)
			if err != nil {
				errs.Add(err)
			}
		} else if stmt.Expr != nil {
			typ = expr.Type()
		} else {
			errs.Add(stmt.WrapError(fmt.Errorf("variable declaration must have at least a type or expression")))
		}

		v := NewVariable(string(stmt.Name.Str), typ, scope)
		v.Position = stmt.Position
		v.expr = expr

		scope := scope.next()

		scope.put(v)

		return &VarStatement{
			Variable:   v,
			Expression: expr,
			Type:       typ,

			Position: stmt.Position,
		}, scope, nil
	case parser.DeclarationStatement:
		expr, err := c.compileExpression(scope, stmt.Expr)
		if err != nil {
			errs.Add(err)
		}

		if expr.Type().Kind() != kinds.Tuple && len(stmt.Names) > 1 {
			errs.Add(stmt.WrapError(fmt.Errorf("cannot destructure non-tuple type %s into %d variables", expr.Type(), len(stmt.Names))))
		}

		var variables []*Variable
		tupleTyp, isTuple := types.Resolve(expr.Type()).(*types.Tuple)

		if isTuple && len(stmt.Names) > 1 {
			for i, name := range stmt.Names {
				var varType types.Type = types.Unknown
				if i < len(tupleTyp.Elems()) {
					varType = tupleTyp.Elems()[i]
				}

				v := NewVariable(string(name.Str), varType, scope)
				v.Position = name.Position

				variables = append(variables, v)

				scope.put(v)
			}
		} else {
			v := NewVariable(string(stmt.Names[0].Str), expr.Type(), scope)
			v.Position = stmt.Names[0].Position

			variables = append(variables, v)

			scope.put(v)
		}

		scope := scope.next()

		return &DeclarationStatement{
			Variables:  variables,
			Expression: expr,

			Position: stmt.Position,
		}, scope, nil
	case parser.AssignmentOperatorStatement:
		left, err := c.compileExpression(scope, stmt.Left)
		if err != nil {
			errs.Add(err)
		}

		right, err := c.compileExpression(scope, stmt.Right)
		if err != nil {
			errs.Add(err)
		}

		return &AssignmentOperatorStatement{
			Left:     left,
			Operator: stmt.Operator,
			Right:    right,

			Position: stmt.Position,
		}, scope, nil
	case parser.AssignmentStatement:
		left, err := c.compileExpression(scope, stmt.Left)
		if err != nil {
			errs.Add(err)
		}

		right, err := c.compileExpression(scope, stmt.Right)
		if err != nil {
			errs.Add(err)
		}

		return &AssignmentStatement{
			Left:  left,
			Right: right,

			Position: stmt.Position,
		}, scope, nil
	case parser.ExprStatement:
		expr, err := c.compileExpression(scope, stmt.Expr)
		if err != nil {
			errs.Add(err)
		}

		return &ExpressionStatement{
			Expression: expr,

			Position: stmt.Position,
		}, scope, nil
	case parser.ReturnStatement:
		var expr Expression
		if stmt.Expr != nil {
			expr, err = c.compileExpression(scope, stmt.Expr)
			if err != nil {
				errs.Add(err)
			}
		}

		function := scope.Function()
		if function == nil {
			errs.Add(stmt.WrapError(fmt.Errorf("return statement outside function")))
		}

		if expr != nil && function.Return() == types.Void {
			errs.Add(stmt.WrapError(fmt.Errorf("unexpected return value on void function")))
		}

		return &ReturnStatement{
			Expression: expr,

			Function: function,

			Position: stmt.Position,
		}, scope, nil
	case parser.PostfixStatement:
		expr, err := c.compileExpression(scope, stmt.Expr)
		if err != nil {
			errs.Add(err)
		}

		return &PostfixStatement{
			Expression: expr,
			Operator:   stmt.Operator,

			Position: stmt.Position,
		}, scope, nil
	case parser.IfStatement:
		condition, err := c.compileExpression(scope, stmt.Condition)
		if err != nil {
			errs.Add(err)
		}

		bodyScope := newScope(scope, "if")
		body, err := c.compileStatements(bodyScope, stmt.Body)
		if err != nil {
			errs.Add(err)
		}

		var rest Statement
		if stmt.Else != nil {
			rest, _, err = c.compileStatement(scope, stmt.Else)
			if err != nil {
				errs.Add(err)
			}
		}

		return &IfStatement{
			Condition: condition,
			Scope:     bodyScope,
			Body:      body,
			Else:      rest,

			Position: stmt.Position,
		}, scope, nil
	case parser.ElseIfStatement:
		condition, err := c.compileExpression(scope, stmt.Condition)
		if err != nil {
			errs.Add(err)
		}

		bodyScope := newScope(scope, "elseif")
		body, err := c.compileStatements(bodyScope, stmt.Body)
		if err != nil {
			errs.Add(err)
		}

		var rest Statement
		if stmt.Else != nil {
			rest, _, err = c.compileStatement(scope, stmt.Else)
			if err != nil {
				errs.Add(err)
			}
		}

		return &IfStatement{
			Condition: condition,
			Scope:     bodyScope,
			Body:      body,
			Else:      rest,

			Position: stmt.Position,
		}, scope, nil
	case parser.ElseStatement:
		bodyScope := newScope(scope, "else")
		body, err := c.compileStatements(bodyScope, stmt.Body)
		if err != nil {
			errs.Add(err)
		}

		return &IfStatement{
			Scope: bodyScope,
			Body:  body,

			Position: stmt.Position,
		}, scope, nil
	case parser.ForStatement:
		var err error
		bodyScope := newScope(scope, "for")

		var init Statement
		if stmt.Init != nil {
			init, bodyScope, err = c.compileStatement(bodyScope, stmt.Init)
			if err != nil {
				errs.Add(err)
			}
		}

		var cond Expression
		if stmt.Condition != nil {
			cond, err = c.compileExpression(bodyScope, stmt.Condition)
			if err != nil {
				errs.Add(err)
			}
		}

		var step Statement
		if stmt.Step != nil {
			step, _, err = c.compileStatement(bodyScope, stmt.Step)
			if err != nil {
				errs.Add(err)
			}
		}

		body, err := c.compileStatements(bodyScope, stmt.Body)
		if err != nil {
			errs.Add(err)
		}

		return &ForStatement{
			Init:      init,
			Condition: cond,
			Step:      step,

			Scope: scope,
			Body:  body,

			Position: stmt.Position,
		}, scope, nil
	default:
		return nil, nil, stmt.WrapError(fmt.Errorf("unhandled statement %T", stmt))
	}
}

func (c *Compiler) compileStatements(scope *SymbolScope, stmts []parser.Statement) (_ []Statement, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	var ret []Statement
	for _, stmt := range stmts {
		retStmt, nextScope, err := c.compileStatement(scope, stmt)
		if err != nil {
			errs.Add(err)
		}

		ret = append(ret, retStmt)

		scope = nextScope
	}

	return ret, nil
}

func (c *Compiler) compileExpression(scope *SymbolScope, expr parser.Expr) (_ Expression, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch expr := expr.(type) {
	case parser.FloatLiteral:
		return &Literal{
			value: air.Float(expr.Value),

			typ: types.Kind(kinds.Float),

			Position: expr.Position,
		}, nil
	case parser.IntLiteral:
		return &Literal{
			value: air.Int(expr.Value),

			typ: types.Kind(kinds.Int),

			Position: expr.Position,
		}, nil
	case parser.StringLiteral:
		return &Literal{
			value: air.String(expr.Value),

			typ: types.Kind(kinds.String),

			Position: expr.Position,
		}, nil
	case parser.BoolLiteral:
		return &Literal{
			value: air.Bool(expr.Value),

			typ: types.Kind(kinds.Bool),

			Position: expr.Position,
		}, nil
	case parser.BinaryExpr:
		left, err := c.compileExpression(scope, expr.Left)
		if err != nil {
			errs.Add(err)
		}

		right, err := c.compileExpression(scope, expr.Right)
		if err != nil {
			errs.Add(err)
		}

		return &BinaryExpression{
			Left:     left,
			Operator: expr.Operator,
			Right:    right,

			Position: expr.Position,
		}, nil
	case parser.IdentifierExpr:
		return &SymbolReferenceExpression{
			SymbolReference: &SymbolReference{
				scope: scope,
				name:  string(expr.Identifier.Str),
			},

			Position: expr.Position,
		}, nil
	case parser.CallExpr:
		funcExpr, err := c.compileExpression(scope, expr.Expr)
		if err != nil {
			errs.Add(err)
		}

		var exprs []Expression
		for _, arg := range expr.Args {
			argExpr, err := c.compileExpression(scope, arg)
			if err != nil {
				errs.Add(err)
			}

			exprs = append(exprs, argExpr)
		}

		// TODO: validate argument types

		return &CallExpression{
			Function: funcExpr,
			Args:     exprs,

			Position: expr.Position,
		}, nil
	case parser.DotExpr:
		receiver, err := c.compileExpression(scope, expr.Expr)
		if err != nil {
			errs.Add(err)
		}

		return &DotExpression{
			Receiver: receiver,
			Key:      string(expr.Key.Str),

			Position: expr.Position,
		}, nil
	case parser.IndexExpr:
		receiver, err := c.compileExpression(scope, expr.Expr)
		if err != nil {
			errs.Add(err)
		}

		index, err := c.compileExpression(scope, expr.Index)
		if err != nil {
			errs.Add(err)
		}

		return &IndexExpression{
			Receiver: receiver,
			Index:    index,

			Position: expr.Position,
		}, nil
	case parser.UnaryExpr:
		exp, err := c.compileExpression(scope, expr.Expr)
		if err != nil {
			errs.Add(err)
		}

		return &UnaryExpression{
			Expression: exp,
			Operator:   operators.Operator(expr.Operator),

			Position: expr.Position,
		}, nil
	case parser.ParenthesizedExpr:
		exp, err := c.compileExpression(scope, expr.Expr)
		if err != nil {
			errs.Add(err)
		}

		return &ParenthesizedExpression{
			Expression: exp,

			Position: expr.Position,
		}, nil
	case parser.TupleExpr:
		var elems []Expression
		for _, elem := range expr.Elems {
			elemExpr, err := c.compileExpression(scope, elem)
			if err != nil {
				errs.Add(err)
			}

			elems = append(elems, elemExpr)
		}

		return &TupleExpression{
			Elems: elems,

			Position: expr.Position,
		}, nil
	case parser.TypeLiteralExpr:
		typeRef, err := c.compileTypeReference(scope, expr.Type)
		if err != nil {
			errs.Add(err)
		}

		var elems []Expression
		for _, elem := range expr.Elems {
			elemExpr, err := c.compileExpression(scope, elem)
			if err != nil {
				errs.Add(err)
			}

			elems = append(elems, elemExpr)
		}

		switch typeRef := types.Resolve(typeRef).(type) {
		case *types.Array:
			if typeRef.Length() == -1 {
				typeRef.SetLength(len(elems))
			}

			if len(elems) != typeRef.Length() {
				errs.Add(expr.WrapError(fmt.Errorf("type %s literal expected %d elements, got %d", typeRef, typeRef.Length(), len(elems))))
			}
		}

		return &TypeLiteralExpression{
			typ:   typeRef,
			Elems: elems,

			Position: expr.Position,
		}, nil
	case parser.TypeExpr:
		typ, err := c.compileTypeReference(scope, expr.Type)
		if err != nil {
			errs.Add(err)
		}

		return &TypeExpression{
			typ: typ,

			Position: expr.Position,
		}, nil
	case parser.SpreadExpr:
		subExpr, err := c.compileExpression(scope, expr.Expr)
		if err != nil {
			errs.Add(err)
		}

		return &SpreadExpression{
			Expr:     subExpr,
			Position: expr.Position,
		}, nil
	case parser.ErrorReturnExpr:
		if scope.Function() == nil {
			errs.Add(expr.WrapError(fmt.Errorf("error return expression is invalid outside function")))
		}

		subExpr, err := c.compileExpression(scope, expr.Expr)
		if err != nil {
			errs.Add(err)
		}

		return &ErrorReturnExpression{
			Expr: subExpr,

			Function: scope.Function(),

			Position: expr.Position,
		}, nil
	case parser.ErrorHandlerExpr:
		if scope.Function() == nil {
			errs.Add(expr.WrapError(fmt.Errorf("error handler expression is invalid outside function")))
		}

		handlerExpr, err := c.compileExpression(scope, expr.Handler)
		if err != nil {
			errs.Add(err)
		}

		errHandler := NewVariable(errorHandlerSymbolName, errorHandlerFunctionType, scope)
		errHandler.Position = expr.Position
		errHandler.expr = handlerExpr
		scope.put(errHandler)

		handlerScope := scope.next()

		subExpr, err := c.compileExpression(handlerScope, expr.Expr)
		if err != nil {
			errs.Add(err)
		}

		return &ErrorHandlerExpression{
			Function: scope.Function(),
			Expr:     subExpr,
			Handler:  handlerExpr,

			Position: expr.Position,
		}, nil
	default:
		return &UnknownExpression{
			ErrorWrapper: expr,
		}, expr.WrapError(fmt.Errorf("unhandled expression %s", expr))
	}
}
