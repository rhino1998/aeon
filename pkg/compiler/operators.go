package compiler

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler/air"
	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/operators"
	"github.com/rhino1998/aeon/pkg/compiler/types"
	"github.com/rhino1998/aeon/pkg/parser"
)

type BinaryExpression struct {
	Left     Expression
	Operator operators.Operator
	Right    Expression

	parser.Position
}

func (e *BinaryExpression) Evaluate() (air.Literal, error) {
	_, err := air.ValidateBinaryExpression(e.Left.Type(), e.Operator, e.Right.Type())
	if err != nil {
		return nil, err
	}

	leftConst, ok := e.Left.(ConstantExpression)
	if !ok {
		return nil, e.Left.WrapError(fmt.Errorf("expression is not constant %s", e.Left))
	}
	left, err := leftConst.Evaluate()
	if err != nil {
		return nil, err
	}

	rightConst, ok := e.Right.(ConstantExpression)
	if !ok {
		return nil, e.Right.WrapError(fmt.Errorf("expression is not constant %s", e.Right))
	}
	right, err := rightConst.Evaluate()
	if err != nil {
		return nil, err
	}

	return air.EvaluateBinaryOperator(left, e.Operator, right)
}

func (e *BinaryExpression) Type() types.Type {
	switch e.Operator {
	case operators.Equal, operators.NotEqual, operators.LessThan, operators.LessThanOrEqual, operators.GreaterThan, operators.GreaterThanOrEqual:
		return types.Kind(kinds.Bool)
	default:
		typ, err := air.ValidateBinaryExpression(e.Left.Type(), e.Operator, e.Right.Type())
		if err != nil {
			return typ
		}

		return typ
	}
}

func (e *BinaryExpression) SymbolDependencies(path []*SymbolReference) []*SymbolReference {
	return append(
		append([]*SymbolReference{},
			e.Left.SymbolDependencies(path)...),
		e.Right.SymbolDependencies(path)...,
	)
}

type AssignmentOperatorStatement struct {
	Left     Expression
	Operator operators.Operator
	Right    Expression

	parser.Position
}

type UnaryExpression struct {
	Expression Expression
	Operator   operators.Operator

	parser.Position
}

func (e *UnaryExpression) Type() types.Type {
	typ, err := air.ValidateUnaryExpression(e.Expression.Type(), e.Operator)
	if err != nil {
		return types.Unknown
	}

	return typ
}

func (e *UnaryExpression) SymbolDependencies(path []*SymbolReference) []*SymbolReference {
	return e.Expression.SymbolDependencies(path)
}
