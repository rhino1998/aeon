package compiler

import (
	"errors"
	"fmt"

	"github.com/rhino1998/aeon/pkg/parser"
)

func (c *Compiler) compileFile(prog *Program, filename string, entry parser.File) error {
	pkg := prog.AddPackage(string(entry.Package.Name))

	for _, decl := range entry.Declarations {
		switch decl := decl.(type) {
		case parser.FunctionDeclaration:
			pkg.scope.put(SymbolStub(decl.Name))
		case parser.TypeDeclaration:
			pkg.scope.put(SymbolStub(decl.Name))
		case parser.ExternFunctionDeclaration:
			pkg.scope.put(SymbolStub(decl.Name))
		case parser.VarDeclaration:
			pkg.scope.put(SymbolStub(decl.Name))
		case parser.ConstDeclaration:
			pkg.scope.put(SymbolStub(decl.Name))
		}
	}

	for _, decl := range entry.Declarations {
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
		switch dir.Name {
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
		name: string(decl.Name),
	}

	for _, param := range decl.Parameters {
		var paramName string
		if param.Name != nil {
			paramName = string(*param.Name)
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
		name:       string(decl.Name),
		underlying: underlying,
	}

	err = scope.put(t)
	if err != nil {
		return nil, decl.WrapError(err)
	}

	return t, nil
}

func (c *Compiler) compileVarDeclaration(p *Package, scope *SymbolScope, decl parser.VarDeclaration) (*Variable, error) {
	v := &Variable{
		name:   string(decl.Name),
		global: true,
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
		name: string(decl.Name),
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
		return nil, decl.WrapError(fmt.Errorf("const declaration must have a type or an expression"))
	}

	err := scope.put(v)
	if err != nil {
		return nil, decl.WrapError(err)
	}

	return v, nil
}

func (c *Compiler) compileTypeReference(scope *SymbolScope, typ parser.Type) (Type, error) {
	switch typ := typ.(type) {
	case parser.Identifier:
		return &ReferencedType{s: scope, name: string(typ)}, nil
	case parser.PointerType:
		pointee, err := c.compileTypeReference(scope, typ.Pointee)
		if err != nil {
			return nil, err
		}

		return &PointerType{
			pointee: pointee,
		}, nil
	case parser.SliceType:
		elem, err := c.compileTypeReference(scope, typ.Element)
		if err != nil {
			return nil, err
		}

		return &SliceType{
			elem: elem,
		}, nil
	case parser.MapType:
		key, err := c.compileTypeReference(scope, typ.Key)
		if err != nil {
			return nil, err
		}

		val, err := c.compileTypeReference(scope, typ.Key)
		if err != nil {
			return nil, err
		}

		return &MapType{
			key:   key,
			value: val,
		}, nil
	case parser.TupleType:
		var elems []Type
		for _, elem := range typ.Elements {
			elemTyp, err := c.compileTypeReference(scope, elem)
			if err != nil {
				return nil, err
			}

			elems = append(elems, elemTyp)
		}

		return &TupleType{
			elems: elems,
		}, nil
	default:
		return nil, fmt.Errorf("unhandled type reference %q", typ)
	}
}

func (c *Compiler) compileFunction(p *Package, scope *SymbolScope, decl parser.FunctionDeclaration) (*Function, error) {
	f := newFunction(string(decl.Name), p)

	for _, param := range decl.Parameters {
		var paramName string
		if param.Name != nil {
			paramName = string(*param.Name)
		}

		typ, err := c.compileTypeReference(scope, param.Type)
		if err != nil {
			return nil, err
		}

		variable := &Variable{
			name: paramName,
			typ:  typ,
		}

		if param.Name != nil {
			f.symbols.put(variable)
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

	f.symbols.put(f)

	for _, stmt := range decl.Body {
		compiledStmt, err := c.compileStatement(f.symbols, stmt)
		if err != nil {
			return nil, err
		}

		f.body = append(f.body, compiledStmt)
	}

	err := scope.put(f)
	if err != nil {
		return nil, err
	}

	return f, nil
}

func (c *Compiler) compileStatement(scope *SymbolScope, stmt parser.Statement) (Statement, error) {
	var err error

	switch stmt := stmt.(type) {
	case *parser.VarStatement:
		var expr Expression
		if stmt.Expr != nil {
			expr, err = c.compileExpression(scope, *stmt.Expr)
			if err != nil {
				return nil, err

			}
		}

		var typ Type
		if stmt.Type != nil {
			typ, err = c.compileTypeReference(scope, *stmt.Type)
			if err != nil {
				return nil, err
			}
		} else if stmt.Expr != nil {
			typ = expr.Type()
		} else {
			return nil, fmt.Errorf("invalid variable declaration")
		}

		v := &Variable{
			name: string(stmt.Name),
			typ:  typ,
		}

		scope.put(v)

		return &VarStatement{
			Variable:   v,
			Expression: expr,
			Type:       typ,

			Position: stmt.Position,
		}, nil
	case parser.DeclarationStatement:
		expr, err := c.compileExpression(scope, stmt.Expr)
		if err != nil {
			return nil, err
		}

		v := &Variable{
			name: string(stmt.Name),
			typ:  expr.Type(),
		}

		scope.put(v)

		return &DeclarationStatement{
			Variable:   v,
			Expression: expr,

			Position: stmt.Position,
		}, nil
	case parser.AssignmentOperatorStatement:
		left, err := c.compileExpression(scope, stmt.Left)
		if err != nil {
			return nil, err
		}

		right, err := c.compileExpression(scope, stmt.Right)
		if err != nil {
			return nil, err
		}

		return &AssignmentOperatorStatement{
			Left:     left,
			Operator: Operator(stmt.Operator),
			Right:    right,

			Position: stmt.Position,
		}, nil
	case parser.AssignmentStatement:
		left, err := c.compileExpression(scope, stmt.Left)
		if err != nil {
			return nil, err
		}

		right, err := c.compileExpression(scope, stmt.Right)
		if err != nil {
			return nil, err
		}

		return &AssignmentStatement{
			Left:  left,
			Right: right,

			Position: stmt.Position,
		}, nil
	case parser.ExprStatement:
		expr, err := c.compileExpression(scope, stmt.Expr)
		if err != nil {
			return nil, err
		}

		return &ExpressionStatement{
			Expression: expr,

			Position: stmt.Position,
		}, nil
	case parser.ReturnStatement:
		var expr Expression
		if stmt.Expr != nil {
			expr, err = c.compileExpression(scope, stmt.Expr)
			if err != nil {
				return nil, err
			}
		}

		function := scope.Function()
		if function == nil {
			return nil, stmt.WrapError(fmt.Errorf("return statement outside function"))
		}

		return &ReturnStatement{
			Expression: expr,

			Position: stmt.Position,
		}, nil
	case parser.PostfixStatement:
		expr, err := c.compileExpression(scope, stmt.Expr)
		if err != nil {
			return nil, err
		}

		return &PostfixStatement{
			Expression: expr,
			Operator:   Operator(stmt.Operator),

			Position: stmt.Position,
		}, nil
	case parser.IfStatement:
		condition, err := c.compileExpression(scope, stmt.Condition)
		if err != nil {
			return nil, err
		}

		bodyScope := newScope(scope, "if")
		body, err := c.compileStatements(bodyScope, stmt.Body)
		if err != nil {
			return nil, err
		}

		var rest Statement
		if stmt.Else != nil {
			rest, err = c.compileStatement(scope, stmt.Else)
			if err != nil {
				return nil, err
			}
		}

		return &IfStatement{
			Condition: condition,
			Scope:     bodyScope,
			Body:      body,
			Else:      rest,

			Position: stmt.Position,
		}, nil
	case parser.ElseIfStatement:
		condition, err := c.compileExpression(scope, stmt.Condition)
		if err != nil {
			return nil, err
		}

		bodyScope := newScope(scope, "elseif")
		body, err := c.compileStatements(bodyScope, stmt.Body)
		if err != nil {
			return nil, err
		}

		var rest Statement
		if stmt.Else != nil {
			rest, err = c.compileStatement(scope, stmt.Else)
			if err != nil {
				return nil, err
			}
		}

		return &IfStatement{
			Condition: condition,
			Scope:     bodyScope,
			Body:      body,
			Else:      rest,

			Position: stmt.Position,
		}, nil
	case parser.ElseStatement:
		bodyScope := newScope(scope, "else")
		body, err := c.compileStatements(bodyScope, stmt.Body)
		if err != nil {
			return nil, err
		}

		return &IfStatement{
			Scope: bodyScope,
			Body:  body,

			Position: stmt.Position,
		}, nil
	case parser.ForStatement:
		var err error
		bodyScope := newScope(scope, "for")

		var init Statement
		if stmt.Init != nil {
			init, err = c.compileStatement(bodyScope, stmt.Init)
			if err != nil {
				return nil, err
			}
		}

		var cond Expression
		if stmt.Condition != nil {
			cond, err = c.compileExpression(bodyScope, stmt.Condition)
			if err != nil {
				return nil, err
			}
		}

		var step Statement
		if stmt.Step != nil {
			step, err = c.compileStatement(bodyScope, stmt.Step)
			if err != nil {
				return nil, err
			}
		}

		body, err := c.compileStatements(bodyScope, stmt.Body)
		if err != nil {
			return nil, err
		}

		return &ForStatement{
			Init:      init,
			Condition: cond,
			Step:      step,

			Scope: scope,
			Body:  body,

			Position: stmt.Position,
		}, nil
	default:
		return nil, stmt.WrapError(fmt.Errorf("unhandled statement %T", stmt))
	}
}

func (c *Compiler) compileStatements(scope *SymbolScope, stmts []parser.Statement) ([]Statement, error) {
	var ret []Statement
	for _, stmt := range stmts {
		retStmt, err := c.compileStatement(scope, stmt)
		if err != nil {
			return nil, err
		}

		ret = append(ret, retStmt)
	}

	return ret, nil
}

func (c *Compiler) compileExpression(scope *SymbolScope, expr parser.Expr) (Expression, error) {
	switch expr := expr.(type) {
	case parser.FloatLiteral:
		return &Literal[Float]{
			value: Float(expr.Value),

			typ: TypeKind(KindFloat),

			Position: expr.Position,
		}, nil
	case parser.IntLiteral:
		return &Literal[Int]{
			value: Int(expr.Value),

			typ: TypeKind(KindInt),

			Position: expr.Position,
		}, nil
	case parser.StringLiteral:
		return &Literal[String]{
			value: String(expr.Value),

			typ: TypeKind(KindString),

			Position: expr.Position,
		}, nil
	case parser.BoolLiteral:
		return &Literal[Bool]{
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
			name:  string(expr.Identifier),

			Position: expr.Position,
		}, nil
	case parser.CallExpr:
		funcExpr, err := c.compileExpression(scope, expr.Expr)
		if err != nil {
			return nil, err
		}

		var exprs []Expression
		for i, arg := range expr.Args {
			argExpr, err := c.compileExpression(scope, arg)
			if err != nil {
				return nil, fmt.Errorf("failed to compile argument %d expression: %w", i, err)
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
			return nil, fmt.Errorf("failed to compile dot expression: %w", err)
		}

		return &DotExpression{
			Receiver: receiver,
			Key:      string(expr.Key),

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
	default:
		return nil, expr.WrapError(fmt.Errorf("unhandled expression %T", expr))
	}
}
