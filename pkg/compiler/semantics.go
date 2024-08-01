package compiler

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/parser"
)

func (c *Compiler) compileFile(prog *Program, entry parser.File) error {

	pkg, ok := prog.root.getPackage(string(entry.Package.Name))
	if !ok {
		pkg = NewPackage(prog, string(entry.Package.Name))
		prog.root.put(pkg)
	}

	for _, decl := range entry.Declarations {
		switch decl := decl.(type) {
		case parser.FunctionDeclaration:
			err := c.compileFunction(pkg, pkg.scope, decl)
			if err != nil {
				return err
			}
		case parser.TypeDeclaration:
			err := c.compileTypeDeclaration(pkg, pkg.scope, decl)
			if err != nil {
				return err
			}
		}
	}

	return nil
}

func (c *Compiler) compileTypeDeclaration(p *Package, scope *Scope, decl parser.TypeDeclaration) error {
	underlying, err := c.compileTypeReference(scope, decl.Type)
	if err != nil {
		return err
	}

	t := &DerivedType{
		name:       string(decl.Name),
		underlying: underlying,
	}

	return scope.put(t)
}

func (c *Compiler) compileTypeReference(scope *Scope, typ parser.Type) (Type, error) {
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

func (c *Compiler) compileFunction(p *Package, scope *Scope, decl parser.FunctionDeclaration) error {
	qualifiedName := fmt.Sprintf("%s.%s", scope.name, decl.Name)

	f := &Function{
		name:  string(decl.Name),
		scope: newScope(scope, qualifiedName),
	}
	f.scope.function = f

	for _, param := range decl.Parameters {
		var paramName string
		if param.Name != nil {
			paramName = string(*param.Name)
		}

		typ, err := c.compileTypeReference(scope, param.Type)
		if err != nil {
			return err
		}

		variable := &Variable{
			name: paramName,
			typ:  typ,
		}

		if param.Name != nil {
			f.scope.put(variable)
		}

		f.parameters = append(f.parameters, variable)
	}

	if decl.Return != nil {
		typ, err := c.compileTypeReference(scope, decl.Return)
		if err != nil {
			return err
		}

		f.ret = typ

	}

	for _, stmt := range decl.Body {
		compiledStmt, err := c.compileStatement(f.scope, stmt)
		if err != nil {
			return err
		}

		f.body = append(f.body, compiledStmt)
	}

	return scope.put(f)
}

func (c *Compiler) compileStatement(scope *Scope, stmt parser.Statement) (Statement, error) {
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

		return &VariableStatement{
			Variable:   *v,
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
			Variable:   *v,
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

		return &ElseIfStatement{
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

		return &ElseStatement{
			Scope: bodyScope,
			Body:  body,

			Position: stmt.Position,
		}, nil
	default:
		return nil, stmt.WrapError(fmt.Errorf("unhandled statement %T", stmt))
	}
}

func (c *Compiler) compileStatements(scope *Scope, stmts []parser.Statement) ([]Statement, error) {
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

func (c *Compiler) compileExpression(scope *Scope, expr parser.Expr) (Expression, error) {
	switch expr := expr.(type) {
	case parser.NumberLiteral:
		if expr.IsInteger() {
			return &Literal[int64]{
				value: int64(expr.Value),
				typ:   KindType(KindInt),

				Position: expr.Position,
			}, nil
		} else {
			return &Literal[float64]{
				value: expr.Value,

				typ: KindType(KindFloat),

				Position: expr.Position,
			}, nil
		}
	case parser.StringLiteral:
		return &Literal[string]{
			value: expr.Value,

			typ: KindType(KindString),

			Position: expr.Position,
		}, nil
	case parser.BooleanLiteral:
		return &Literal[bool]{
			value: expr.Value,

			typ: KindType(KindBool),

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
			function: funcExpr,
			args:     exprs,

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

		typ, err := validateUnaryExpression(exp.Type(), Operator(expr.Operator))
		if err != nil {
			return nil, expr.Position.WrapError(err)
		}

		return &UnaryExpression{
			Expression: exp,
			Operator:   Operator(expr.Operator),

			typ: typ,

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
