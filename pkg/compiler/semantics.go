package compiler

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/parser"
)

type Program struct {
	root *Scope
}

func newProgram() *Program {
	p := &Program{
		root: builtins(),
	}

	return p
}

func (c *Compiler) compileProgram(entry parser.Program) (*Program, error) {
	p := newProgram()

	packageScope := newScope(p.root, string(entry.Package.Name))
	p.root.put(packageScope)

	for _, decl := range entry.Declarations {
		switch decl := decl.(type) {
		case parser.FunctionDeclaration:
			err := c.compileFunction(p, packageScope, decl)
			if err != nil {
				return nil, fmt.Errorf("failed to compile function %q: %w", decl.Name, err)
			}
		case parser.TypeDeclaration:
			err := c.compileTypeDeclaration(p, packageScope, decl)
			if err != nil {
				return nil, fmt.Errorf("failed to compile type declaration %q: %w", decl.Name, err)
			}
		}
	}

	return p, nil
}

func (c *Compiler) compileTypeDeclaration(p *Program, scope *Scope, decl parser.TypeDeclaration) error {
	underlying, err := c.compileTypeReference(scope, decl.Type)
	if err != nil {
		return fmt.Errorf("failed to compile type reference: %w", err)
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
		return ReferencedType{s: scope, name: string(typ)}, nil
	case parser.PointerType:
		pointee, err := c.compileTypeReference(scope, typ.Pointee)
		if err != nil {
			return nil, fmt.Errorf("invalid pointer type: %w", err)
		}

		return &PointerType{
			pointee: pointee,
		}, nil
	case parser.SliceType:
		elem, err := c.compileTypeReference(scope, typ.Element)
		if err != nil {
			return nil, fmt.Errorf("invalid pointer type: %w", err)
		}

		return &SliceType{
			elem: elem,
		}, nil
	case parser.MapType:
		key, err := c.compileTypeReference(scope, typ.Key)
		if err != nil {
			return nil, fmt.Errorf("invalid map key type: %w", err)
		}

		val, err := c.compileTypeReference(scope, typ.Key)
		if err != nil {
			return nil, fmt.Errorf("invalid map value type: %w", err)
		}

		return &MapType{
			key:   key,
			value: val,
		}, nil
	case parser.TupleType:
		var elems []Type
		for i, elem := range typ.Elements {
			elemTyp, err := c.compileTypeReference(scope, elem)
			if err != nil {
				return nil, fmt.Errorf("invalid tuple element type at index %d: %w", i, err)
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

func (c *Compiler) compileFunction(p *Program, scope *Scope, decl parser.FunctionDeclaration) error {
	qualifiedName := fmt.Sprintf("%s.%s", scope.name, decl.Name)

	f := &Function{
		name:  string(decl.Name),
		scope: newScope(scope, qualifiedName),
	}

	for i, param := range decl.Parameters {
		var paramName string
		if param.Name != nil {
			paramName = string(*param.Name)
		}

		typ, err := c.compileTypeReference(scope, param.Type)
		if err != nil {
			return fmt.Errorf("invalid type reference in parameter %d %q: %w", i, paramName, err)
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
			return fmt.Errorf("invalid type reference in return value: %w", err)
		}

		f.ret = typ

	}

	for i, stmt := range decl.Body {
		compiledStmt, err := c.compileStatement(scope, stmt)
		if err != nil {
			return fmt.Errorf("failed to compile statement %d: %w", i, err)
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
				return nil, fmt.Errorf("failed to compile expression: %w", err)
			}
		}

		var typ Type
		if stmt.Type != nil {
			typ, err = c.compileTypeReference(scope, *stmt.Type)
			if err != nil {
				return nil, fmt.Errorf("failed to compile type reference: %w", err)
			}

			// TODO: check equality
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
			expression: expr,
		}, nil
	case parser.DeclarationStatement:
		expr, err := c.compileExpression(scope, stmt.Expr)
		if err != nil {
			return nil, fmt.Errorf("failed to compile expression: %w", err)
		}

		v := &Variable{
			name: string(stmt.Name),
			typ:  expr.Type(),
		}

		scope.put(v)

		return &DeclarationStatement{
			Variable:   *v,
			expression: expr,
		}, nil
	case parser.AssignmentOperatorStatement:
		left, err := c.compileExpression(scope, stmt.Left)
		if err != nil {
			return nil, fmt.Errorf("failed to compile lhs expression: %w", err)
		}

		right, err := c.compileExpression(scope, stmt.Right)
		if err != nil {
			return nil, fmt.Errorf("failed to compile rhs expression: %w", err)
		}

		_, err = validateBinaryExpression(left.Type(), Operator(stmt.Operator), right.Type())
		if err != nil {
			return nil, fmt.Errorf("invalid assignment %q: %w", stmt.Operator, err)
		}

		return &AssignmentOperatorStatement{
			left:     left,
			operator: Operator(stmt.Operator),
			right:    right,
		}, nil
	case parser.AssignmentStatement:
		left, err := c.compileExpression(scope, stmt.Left)
		if err != nil {
			return nil, fmt.Errorf("failed to compile lhs expression: %w", err)
		}

		right, err := c.compileExpression(scope, stmt.Right)
		if err != nil {
			return nil, fmt.Errorf("failed to compile rhs expression: %w", err)
		}

		if !TypesEqual(left.Type(), right.Type()) {
			return nil, fmt.Errorf("mismatched types for assignment: %T vs. %T", left.Type(), right.Type())
		}

		return &AssignmentStatement{
			left:  left,
			right: right,
		}, nil
	case parser.ExprStatement:
		expr, err := c.compileExpression(scope, stmt.Expr)
		if err != nil {
			return nil, fmt.Errorf("failed to compile expression: %w", err)
		}

		return expr, nil
	default:
		return nil, fmt.Errorf("unhandled statement %T", stmt)
	}
}

func (c *Compiler) compileExpression(scope *Scope, expr parser.Expr) (Expression, error) {
	switch expr := expr.(type) {
	case parser.NumberLiteral:
		if expr.IsInteger() {
			return &NumericLiteral{
				value: float64(expr),
				typ:   IntType,
			}, nil
		} else {
			return &NumericLiteral{
				value: float64(expr),
				typ:   FloatType,
			}, nil
		}
	case parser.StringLiteral:
		return &StringLiteral{
			value: string(expr),
			typ:   StringType,
		}, nil
	case parser.BooleanLiteral:
		return &BooleanLiteral{
			value: bool(expr),
			typ:   BoolType,
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

		typ, err := validateBinaryExpression(left.Type(), Operator(expr.Operator), right.Type())
		if err != nil {
			return nil, fmt.Errorf("invalid binary expression: %w", err)
		}

		return &BinaryExpression{
			left:     left,
			operator: Operator(expr.Operator),
			right:    right,
			typ:      typ,
		}, nil
	case parser.IdentifierExpr:
		return &VariableReferenceExpression{
			scope: scope,
			name:  string(expr.Identifier),
		}, nil
	case parser.CallExpr:

		funcExpr, err := c.compileExpression(scope, expr.Expr)
		if err != nil {
			return nil, fmt.Errorf("failed to compile function expression: %w", err)
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
		}, nil
	case parser.DotExpr:
		receiver, err := c.compileExpression(scope, expr.Expr)
		if err != nil {
			return nil, fmt.Errorf("failed to compile dot expression: %w", err)
		}

		return &DotExpression{
			Receiver: receiver,
			Key:      string(expr.Key),
		}, nil
	default:
		return nil, fmt.Errorf("unhandled expresion %T", expr)
	}
}
