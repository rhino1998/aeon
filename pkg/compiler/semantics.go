package compiler

import (
	"errors"
	"fmt"

	"github.com/rhino1998/aeon/pkg/parser"
)

func (c *Compiler) compileFile(prog *Program, filename string, entry parser.File) error {
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
			var posError parser.PositionError
			if !errors.As(err, &posError) {
				return FileError{File: filename, Err: err}
			}

			return err
		}
	}

	// TODO: sort topologically
	for _, decl := range entry.Declarations {
		if _, ok := decl.(parser.TypeDeclaration); ok {
			continue
		}

		_, err := c.compileDeclaration(pkg, pkg.scope, decl)
		if err != nil {
			var posError parser.PositionError
			if !errors.As(err, &posError) {
				return FileError{File: filename, Err: err}
			}

			return err
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

func (c *Compiler) compileDirective(p *Package, scope *SymbolScope, dir parser.Directive) (Symbol, error) {
	sym, err := c.compileDeclaration(p, scope, dir.Declaration)
	if err != nil {
		return nil, err
	}

	switch sym := sym.(type) {
	case *Function:
		switch dir.Name.Str {
		case "init":
			if len(dir.Args) != 0 {
				return nil, dir.WrapError(fmt.Errorf("expected 0 arguments for directive %q", dir.Name))
			}

			p.initFuncs = append(p.initFuncs, sym)
		case "update":
			if len(dir.Args) != 0 {
				return nil, dir.WrapError(fmt.Errorf("expected 0 arguments for directive %q", dir.Name))
			}

			p.updateFuncs = append(p.updateFuncs, sym)
		default:
			return nil, dir.WrapError(fmt.Errorf("unknown directive %q", dir.Name))
		}
	case *ExternFunction:
		return nil, dir.WrapError(fmt.Errorf("directive %q is not supported for extern declarations", dir.Name))
	case *DerivedType:
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

func (c *Compiler) compileExternFunctionDeclaration(p *Package, scope *SymbolScope, decl parser.ExternFunctionDeclaration) (*ExternFunction, error) {
	f := &ExternFunction{
		name: string(decl.Name.Str),
	}

	for _, param := range decl.Parameters {
		var paramName string
		if param.Name != nil {
			paramName = string(param.Name.Str)
		}

		typ, err := c.compileTypeReference(scope, param.Type)
		if err != nil {
			return nil, err
		}

		variable := &Variable{
			name: paramName,
			typ:  typ,
		}

		f.parameters = append(f.parameters, variable)
	}

	if decl.Return != nil {
		typ, err := c.compileTypeReference(scope, decl.Return)
		if err != nil {
			return nil, err
		}

		f.ret = typ
	} else {
		f.ret = VoidType
	}

	p.prog.externFuncs[f.Name()] = f

	err := scope.put(f)
	if err != nil {
		return nil, decl.WrapError(err)
	}

	return f, nil
}

func (c *Compiler) compileTypeDeclaration(p *Package, scope *SymbolScope, decl parser.TypeDeclaration) (*DerivedType, error) {
	underlying, err := c.compileTypeReference(scope, decl.Type)
	if err != nil {
		return nil, err
	}

	t := &DerivedType{
		name:       string(decl.Name.Str),
		pkg:        p,
		underlying: underlying,

		methodFuncs: make(map[string]*Function),
	}

	err = scope.put(t)
	if err != nil {
		return nil, decl.WrapError(err)
	}

	return t, nil
}

func (c *Compiler) compileVarDeclaration(p *Package, scope *SymbolScope, decl parser.VarDeclaration) (*Variable, error) {
	v := &Variable{
		name:   string(decl.Name.Str),
		global: true,

		Position: decl.Position,
	}
	if decl.Type != nil {
		typ, err := c.compileTypeReference(scope, *decl.Type)
		if err != nil {
			return nil, err
		}

		v.typ = typ
	}

	if decl.Expr != nil {
		expr, err := c.compileExpression(scope, *decl.Expr)
		if err != nil {
			return nil, err
		}

		v.expr = expr

		if v.typ == nil {
			v.typ = expr.Type()
		}
	}

	if v.typ == nil {
		return nil, decl.WrapError(fmt.Errorf("variable declaration must have a type or an expression"))
	}

	err := scope.put(v)
	if err != nil {
		return nil, decl.WrapError(err)
	}

	return v, nil
}

func (c *Compiler) compileConstDeclaration(p *Package, scope *SymbolScope, decl parser.ConstDeclaration) (*Constant, error) {
	v := &Constant{
		name: string(decl.Name.Str),

		Position: decl.Position,
	}
	if decl.Type != nil {
		typ, err := c.compileTypeReference(scope, *decl.Type)
		if err != nil {
			return nil, err
		}

		v.typ = typ
	}

	if decl.Expr != nil {
		expr, err := c.compileExpression(scope, *decl.Expr)
		if err != nil {
			return nil, err
		}

		v.ConstantExpression = expr.(ConstantExpression)

		if v.typ == nil {
			v.typ = expr.Type()
		}
	}

	if v.typ == nil {
		return nil, decl.WrapError(fmt.Errorf("const declaration must have a type or an expression"))
	}

	err := scope.put(v)
	if err != nil {
		return nil, decl.WrapError(err)
	}

	return v, nil
}

func (c *Compiler) compileTypeReference(scope *SymbolScope, typ parser.Type) (_ Type, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	switch typ := typ.(type) {
	case nil:
		return UnknownType, nil
	case parser.Identifier:
		return &ReferencedType{s: scope, name: string(typ.Str)}, nil
	case parser.PointerType:
		pointee, err := c.compileTypeReference(scope, typ.Pointee)
		if err != nil {
			errs.Add(err)
		}

		return &PointerType{
			pointee: pointee,

			Position: typ.Position,
		}, nil
	case parser.SliceType:
		elem, err := c.compileTypeReference(scope, typ.Element)
		if err != nil {
			errs.Add(err)
		}

		return &SliceType{
			elem: elem,

			Position: typ.Position,
		}, nil
	case parser.MapType:
		key, err := c.compileTypeReference(scope, typ.Key)
		if err != nil {
			errs.Add(err)
		}

		val, err := c.compileTypeReference(scope, typ.Key)
		if err != nil {
			errs.Add(err)
		}

		return &MapType{
			key:   key,
			value: val,

			Position: typ.Position,
		}, nil
	case parser.TupleType:
		var elems []Type
		for _, elem := range typ.Elements {
			elemTyp, err := c.compileTypeReference(scope, elem)
			if err != nil {
				errs.Add(err)
			}

			elems = append(elems, elemTyp)
		}

		return &TupleType{
			elems: elems,

			Position: typ.Position,
		}, nil
	case parser.ArrayType:
		elem, err := c.compileTypeReference(scope, typ.Element)
		if err != nil {
			errs.Add(err)
		}

		return &ArrayType{
			length: int(typ.Length.Value),
			elem:   elem,

			Position: typ.Position,
		}, nil
	case parser.StructType:
		var fields []StructField
		for _, field := range typ.Fields {
			fieldType, err := c.compileTypeReference(scope, field.Type)
			if err != nil {
				errs.Add(err)
			}

			fields = append(fields, StructField{
				Name: string(field.Name.Str),
				Type: fieldType,
			})
		}

		return &StructType{
			fields: fields,

			Position: typ.Position,
		}, nil
	case parser.FunctionType:
		var lastParamType parser.Type
		parameters := make([]Type, len(typ.Parameters))
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
		var ret Type = VoidType
		if typ.Return != nil {
			ret, err = c.compileTypeReference(scope, typ.Return)
			if err != nil {
				errs.Add(err)
			}
		}

		return &FunctionType{
			Receiver:   VoidType,
			Return:     ret,
			Parameters: parameters,
		}, nil
	default:
		return nil, typ.WrapError(fmt.Errorf("unhandled type reference %q", typ))
	}
}

func (c *Compiler) compileFunction(p *Package, scope *SymbolScope, decl parser.FunctionDeclaration) (_ *Function, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	f := newFunction(string(decl.Name.Str), p)

	f.receiver = &Variable{
		typ: VoidType,
	}

	var lastParamType parser.Type
	f.parameters = make([]*Variable, len(decl.Parameters))
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

		variable := &Variable{
			name: paramName,
			typ:  typ,

			Position: param.Position,
		}

		f.parameters[i] = variable
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
		f.ret = VoidType
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

	f := newFunction(string(decl.Name.Str), p)

	var recvName string
	if decl.Receiver.Name != nil {
		recvName = string(decl.Receiver.Name.Str)
	}

	recvTyp, err := c.compileTypeReference(scope, decl.Receiver.Type)
	if err != nil {
		errs.Add(err)
	}

	if !IsValidMethodReceiverType(recvTyp) {
		errs.Add(decl.WrapError(fmt.Errorf("cannot use %s as a method receiver", recvTyp)))
	}

	var recvDerivedType *DerivedType
	switch recvTyp := resolveType(recvTyp).(type) {
	case *DerivedType:
		recvDerivedType = recvTyp
	case *PointerType:
		switch recvTyp := resolveType(recvTyp.Pointee()).(type) {
		case *DerivedType:
			recvDerivedType = recvTyp
		}
	}

	recvVar := &Variable{
		name: recvName,
		typ:  recvTyp,
	}

	f.receiver = recvVar

	if decl.Receiver.Name != nil {
		f.symbols.put(recvVar)
	}

	var lastParamType parser.Type
	f.parameters = make([]*Variable, len(decl.Parameters))
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

		variable := &Variable{
			name: paramName,
			typ:  typ,
		}

		f.parameters[i] = variable
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
		f.ret = VoidType
	}

	f.symbols.put(f)

	f.body, err = c.compileStatements(f.symbols, decl.Body)
	if err != nil {
		errs.Add(err)
	}

	if f.Return() != VoidType {
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

	recvDerivedType.AddMethod(string(decl.Name.Str), f)

	return f, nil
}

func (c *Compiler) compileStatement(scope *SymbolScope, stmt parser.Statement) (Statement, *SymbolScope, error) {
	var err error

	switch stmt := stmt.(type) {
	case *parser.VarStatement:
		var expr Expression
		if stmt.Expr != nil {
			expr, err = c.compileExpression(scope, *stmt.Expr)
			if err != nil {
				return nil, nil, err

			}
		}

		var typ Type
		if stmt.Type != nil {
			typ, err = c.compileTypeReference(scope, *stmt.Type)
			if err != nil {
				return nil, nil, err
			}
		} else if stmt.Expr != nil {
			typ = expr.Type()
		} else {
			return nil, nil, fmt.Errorf("invalid variable declaration")
		}

		v := &Variable{
			name: string(stmt.Name.Str),
			typ:  typ,
		}

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
			return nil, nil, err
		}

		v := &Variable{
			name: string(stmt.Name.Str),
			typ:  expr.Type(),
		}

		scope := scope.next()

		scope.put(v)

		return &DeclarationStatement{
			Variable:   v,
			Expression: expr,

			Position: stmt.Position,
		}, scope, nil
	case parser.AssignmentOperatorStatement:
		left, err := c.compileExpression(scope, stmt.Left)
		if err != nil {
			return nil, nil, err
		}

		right, err := c.compileExpression(scope, stmt.Right)
		if err != nil {
			return nil, nil, err
		}

		return &AssignmentOperatorStatement{
			Left:     left,
			Operator: Operator(stmt.Operator),
			Right:    right,

			Position: stmt.Position,
		}, scope, nil
	case parser.AssignmentStatement:
		left, err := c.compileExpression(scope, stmt.Left)
		if err != nil {
			return nil, nil, err
		}

		right, err := c.compileExpression(scope, stmt.Right)
		if err != nil {
			return nil, nil, err
		}

		return &AssignmentStatement{
			Left:  left,
			Right: right,

			Position: stmt.Position,
		}, scope, nil
	case parser.ExprStatement:
		expr, err := c.compileExpression(scope, stmt.Expr)
		if err != nil {
			return nil, nil, err
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
				return nil, nil, err
			}
		}

		function := scope.Function()
		if function == nil {
			return nil, nil, stmt.WrapError(fmt.Errorf("return statement outside function"))
		}

		return &ReturnStatement{
			Expression: expr,

			Function: function,

			Position: stmt.Position,
		}, scope, nil
	case parser.PostfixStatement:
		expr, err := c.compileExpression(scope, stmt.Expr)
		if err != nil {
			return nil, nil, err
		}

		return &PostfixStatement{
			Expression: expr,
			Operator:   Operator(stmt.Operator),

			Position: stmt.Position,
		}, scope, nil
	case parser.IfStatement:
		condition, err := c.compileExpression(scope, stmt.Condition)
		if err != nil {
			return nil, nil, err
		}

		bodyScope := newScope(scope, "if")
		body, err := c.compileStatements(bodyScope, stmt.Body)
		if err != nil {
			return nil, nil, err
		}

		var rest Statement
		if stmt.Else != nil {
			rest, _, err = c.compileStatement(scope, stmt.Else)
			if err != nil {
				return nil, nil, err
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
			return nil, nil, err
		}

		bodyScope := newScope(scope, "elseif")
		body, err := c.compileStatements(bodyScope, stmt.Body)
		if err != nil {
			return nil, nil, err
		}

		var rest Statement
		if stmt.Else != nil {
			rest, _, err = c.compileStatement(scope, stmt.Else)
			if err != nil {
				return nil, nil, err
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
			return nil, nil, err
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
				return nil, nil, err
			}
		}

		var cond Expression
		if stmt.Condition != nil {
			cond, err = c.compileExpression(bodyScope, stmt.Condition)
			if err != nil {
				return nil, nil, err
			}
		}

		var step Statement
		if stmt.Step != nil {
			step, _, err = c.compileStatement(bodyScope, stmt.Step)
			if err != nil {
				return nil, nil, err
			}
		}

		body, err := c.compileStatements(bodyScope, stmt.Body)
		if err != nil {
			return nil, nil, err
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

func (c *Compiler) compileStatements(scope *SymbolScope, stmts []parser.Statement) ([]Statement, error) {
	var ret []Statement
	for _, stmt := range stmts {
		retStmt, nextScope, err := c.compileStatement(scope, stmt)
		if err != nil {
			return nil, err
		}

		ret = append(ret, retStmt)

		scope = nextScope
	}

	return ret, nil
}

func (c *Compiler) compileExpression(scope *SymbolScope, expr parser.Expr) (Expression, error) {
	switch expr := expr.(type) {
	case parser.FloatLiteral:
		return &Literal{
			value: Float(expr.Value),

			typ: TypeKind(KindFloat),

			Position: expr.Position,
		}, nil
	case parser.IntLiteral:
		return &Literal{
			value: Int(expr.Value),

			typ: TypeKind(KindInt),

			Position: expr.Position,
		}, nil
	case parser.StringLiteral:
		return &Literal{
			value: String(expr.Value),

			typ: TypeKind(KindString),

			Position: expr.Position,
		}, nil
	case parser.BoolLiteral:
		return &Literal{
			value: Bool(expr.Value),

			typ: TypeKind(KindBool),

			Position: expr.Position,
		}, nil
	case parser.BinaryExpr:
		left, err := c.compileExpression(scope, expr.Left)
		if err != nil {
			return nil, fmt.Errorf("failed to compile lhs of expression: %w", err)
		}

		right, err := c.compileExpression(scope, expr.Right)
		if err != nil {
			return nil, fmt.Errorf("failed to compile lhs of expression: %w", err)
		}

		return &BinaryExpression{
			Left:     left,
			Operator: Operator(expr.Operator),
			Right:    right,

			Position: expr.Position,
		}, nil
	case parser.IdentifierExpr:
		return &SymbolReferenceExpression{
			scope: scope,
			name:  string(expr.Identifier.Str),

			Position: expr.Position,
		}, nil
	case parser.CallExpr:
		funcExpr, err := c.compileExpression(scope, expr.Expr)
		if err != nil {
			return nil, err
		}

		var exprs []Expression
		for _, arg := range expr.Args {
			argExpr, err := c.compileExpression(scope, arg)
			if err != nil {
				return nil, err
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
			return nil, err
		}

		return &DotExpression{
			Receiver: receiver,
			Key:      string(expr.Key.Str),

			Position: expr.Position,
		}, nil
	case parser.IndexExpr:
		receiver, err := c.compileExpression(scope, expr.Expr)
		if err != nil {
			return nil, err
		}

		index, err := c.compileExpression(scope, expr.Index)
		if err != nil {
			return nil, err
		}

		return &IndexExpression{
			Receiver: receiver,
			Index:    index,

			Position: expr.Position,
		}, nil
	case parser.UnaryExpr:
		exp, err := c.compileExpression(scope, expr.Expr)
		if err != nil {
			return nil, err
		}

		return &UnaryExpression{
			Expression: exp,
			Operator:   Operator(expr.Operator),

			Position: expr.Position,
		}, nil
	case parser.ParenthesizedExpr:
		exp, err := c.compileExpression(scope, expr.Expr)
		if err != nil {
			return nil, err
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
				return nil, err
			}

			elems = append(elems, elemExpr)
		}

		return &TupleExpression{
			Elems: elems,

			Position: expr.Position,
		}, nil
	case parser.ArrayExpr:
		lengthExpr, err := c.compileExpression(scope, expr.Length)
		if err != nil {
			return nil, err
		}

		lengthLiteral, ok := lengthExpr.(*Literal)
		if !ok {
			return nil, expr.WrapError(fmt.Errorf("array size must be an integer literal"))
		}

		typeRef, err := c.compileTypeReference(scope, expr.Type)
		if err != nil {
			return nil, err
		}

		var elems []Expression
		for _, elem := range expr.Elems {
			elemExpr, err := c.compileExpression(scope, elem)
			if err != nil {
				return nil, err
			}

			elems = append(elems, elemExpr)
		}

		return &ArrayExpression{
			Length:   lengthLiteral,
			ElemType: typeRef,
			Elems:    elems,

			Position: expr.Position,
		}, nil
	default:
		return nil, expr.WrapError(fmt.Errorf("unhandled expression %T", expr))
	}
}
