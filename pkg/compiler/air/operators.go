package air

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/operators"
	"github.com/rhino1998/aeon/pkg/compiler/types"
)

func ValidateBinaryExpression(left types.Type, operator operators.Operator, right types.Type) (types.Type, error) {
	if !types.Equal(left, right) {
		return types.Unknown, fmt.Errorf("invalid binary operator %q for types %q and %q", operator, left, right)
	}

	switch left.Kind() {
	case kinds.Int, kinds.Float, kinds.String, kinds.Pointer, kinds.Bool:
		kind := binaryOperatorKinds[BinaryOperatorKinds{operator, left.Kind(), right.Kind()}]
		if kind == kinds.Unknown {
			return types.Unknown, fmt.Errorf("invalid binary operator %q for types %q and %q", operator, left, right)
		}

		if operator.IsComparison() {
			return types.Kind(kinds.Bool), nil
		}

		return left, nil
	case kinds.Interface:
		if operator == operators.Equal || operator == operators.NotEqual {
			return types.Kind(kinds.Bool), nil
		}

		return types.Unknown, fmt.Errorf("invalid binary operator %q for type %q", operator, left)
	case kinds.Slice:
		if operator == operators.Equal || operator == operators.NotEqual {
			return types.Kind(kinds.Bool), nil
		}

		return types.Unknown, fmt.Errorf("invalid binary operator %q for type %q", operator, left)
	case kinds.Tuple:
		leftTuple := types.Resolve(left).(*types.Tuple)
		rightTuple := types.Resolve(right).(*types.Tuple)
		for i := range leftTuple.Elems() {
			leftElem := leftTuple.Elems()[i]
			rightElem := rightTuple.Elems()[i]
			_, err := ValidateBinaryExpression(leftElem, operator, rightElem)
			if err != nil {
				return types.Unknown, err
			}
		}

		if operator.IsComparison() {
			return types.Kind(kinds.Bool), nil
		}

		return left, nil
	case kinds.Array:
		leftElem := types.Resolve(left).(*types.Array).Elem()
		rightElem := types.Resolve(right).(*types.Array).Elem()
		_, err := ValidateBinaryExpression(leftElem, operator, rightElem)
		if err != nil {
			return types.Unknown, err
		}

		if operator.IsComparison() {
			return types.Kind(kinds.Bool), nil
		}

		return left, nil
	default:
		return types.Unknown, fmt.Errorf("invalid binary operator %q for type %q", operator, left)
	}
}

var binaryOperatorKinds = map[BinaryOperatorKinds]kinds.Kind{
	// **
	{operators.Exponentiation, kinds.Int, kinds.Int}:     kinds.Int,
	{operators.Exponentiation, kinds.Float, kinds.Float}: kinds.Float,

	{operators.LeftShift, kinds.Int, kinds.Int}:  kinds.Int,
	{operators.RightShift, kinds.Int, kinds.Int}: kinds.Int,
	{operators.BitwiseAnd, kinds.Int, kinds.Int}: kinds.Int,
	{operators.BitwiseOr, kinds.Int, kinds.Int}:  kinds.Int,
	{operators.BitwiseXor, kinds.Int, kinds.Int}: kinds.Int,

	// +
	{operators.Addition, kinds.Int, kinds.Int}:       kinds.Int,
	{operators.Addition, kinds.Float, kinds.Float}:   kinds.Float,
	{operators.Addition, kinds.String, kinds.String}: kinds.String,

	// -
	{operators.Subtraction, kinds.Int, kinds.Int}:     kinds.Int,
	{operators.Subtraction, kinds.Float, kinds.Float}: kinds.Float,

	// *
	{operators.Multiplication, kinds.Int, kinds.Int}:     kinds.Int,
	{operators.Multiplication, kinds.Float, kinds.Float}: kinds.Float,

	// /
	{operators.Division, kinds.Int, kinds.Int}:     kinds.Int,
	{operators.Division, kinds.Float, kinds.Float}: kinds.Float,

	// %
	{operators.Modulo, kinds.Int, kinds.Int}: kinds.Int,

	// ==
	{operators.Equal, kinds.Int, kinds.Int}:         kinds.Bool,
	{operators.Equal, kinds.Float, kinds.Float}:     kinds.Bool,
	{operators.Equal, kinds.String, kinds.String}:   kinds.Bool,
	{operators.Equal, kinds.Bool, kinds.Bool}:       kinds.Bool,
	{operators.Equal, kinds.Pointer, kinds.Pointer}: kinds.Bool,
	{operators.Equal, kinds.Type, kinds.Type}:       kinds.Bool,

	// !=
	{operators.NotEqual, kinds.Int, kinds.Int}:         kinds.Bool,
	{operators.NotEqual, kinds.Float, kinds.Float}:     kinds.Bool,
	{operators.NotEqual, kinds.String, kinds.String}:   kinds.Bool,
	{operators.NotEqual, kinds.Bool, kinds.Bool}:       kinds.Bool,
	{operators.NotEqual, kinds.Pointer, kinds.Pointer}: kinds.Bool,
	{operators.Equal, kinds.Type, kinds.Type}:          kinds.Bool,

	// <
	{operators.LessThan, kinds.Int, kinds.Int}:       kinds.Bool,
	{operators.LessThan, kinds.Float, kinds.Float}:   kinds.Bool,
	{operators.LessThan, kinds.String, kinds.String}: kinds.Bool,

	// <=
	{operators.LessThanOrEqual, kinds.Int, kinds.Int}:       kinds.Bool,
	{operators.LessThanOrEqual, kinds.Float, kinds.Float}:   kinds.Bool,
	{operators.LessThanOrEqual, kinds.String, kinds.String}: kinds.Bool,

	// >
	{operators.GreaterThan, kinds.Int, kinds.Int}:       kinds.Bool,
	{operators.GreaterThan, kinds.Float, kinds.Float}:   kinds.Bool,
	{operators.GreaterThan, kinds.String, kinds.String}: kinds.Bool,

	// >=
	{operators.GreaterThanOrEqual, kinds.Int, kinds.Int}:       kinds.Bool,
	{operators.GreaterThanOrEqual, kinds.Float, kinds.Float}:   kinds.Bool,
	{operators.GreaterThanOrEqual, kinds.String, kinds.String}: kinds.Bool,

	{operators.LogicalAnd, kinds.Bool, kinds.Bool}: kinds.Bool,
	{operators.LogicalOr, kinds.Bool, kinds.Bool}:  kinds.Bool,
}

var binaryOperatorEvaluators = map[BinaryOperatorKinds]binaryOperatorEvaluatorFunc{
	{operators.Addition, kinds.Int, kinds.Int}:       binaryOperatorArithmeticEvaluator[Int](binaryOperatorAdd),
	{operators.Addition, kinds.Float, kinds.Float}:   binaryOperatorArithmeticEvaluator[Float](binaryOperatorAdd),
	{operators.Addition, kinds.String, kinds.String}: binaryOperatorArithmeticEvaluator[String](binaryOperatorAdd),

	{operators.Subtraction, kinds.Int, kinds.Int}:     binaryOperatorArithmeticEvaluator[Int](binaryOperatorSub),
	{operators.Subtraction, kinds.Float, kinds.Float}: binaryOperatorArithmeticEvaluator[Float](binaryOperatorSub),

	{operators.Multiplication, kinds.Int, kinds.Int}:     binaryOperatorArithmeticEvaluator[Int](binaryOperatorMult),
	{operators.Multiplication, kinds.Float, kinds.Float}: binaryOperatorArithmeticEvaluator[Float](binaryOperatorMult),

	{operators.Division, kinds.Int, kinds.Int}:     binaryOperatorArithmeticEvaluator[Int](binaryOperatorDiv),
	{operators.Division, kinds.Float, kinds.Float}: binaryOperatorArithmeticEvaluator[Float](binaryOperatorDiv),
}

func EvaluateBinaryOperator(left Literal, op operators.Operator, right Literal) (Literal, error) {
	fun, ok := binaryOperatorEvaluators[BinaryOperatorKinds{op, left.Kind(), right.Kind()}]
	if !ok {
		return nil, fmt.Errorf("invalid binary operator %q for types %q and %q", op, left.Kind(), right.Kind())
	}

	return fun(left, right)
}

func binaryOperatorArithmeticEvaluator[T LiteralBound](f func(T, T) (T, error)) binaryOperatorEvaluatorFunc {
	return func(a, b Literal) (Literal, error) {
		aT, ok := a.(T)
		if !ok {
			return nil, fmt.Errorf("type mismatch")
		}

		bT, ok := b.(T)
		if !ok {
			return nil, fmt.Errorf("type mismatch")
		}

		return f(aT, bT)
	}
}

func binaryOperatorAdd[T interface{ Int | Float | String }](a, b T) (T, error) {
	return a + b, nil
}

func binaryOperatorSub[T interface{ Int | Float }](a, b T) (T, error) {
	return a - b, nil
}

func binaryOperatorMult[T interface{ Int | Float }](a, b T) (T, error) {
	return a * b, nil
}

func binaryOperatorDiv[T interface{ Int | Float }](a, b T) (T, error) {
	return a / b, nil
}

type binaryOperatorEvaluatorFunc func(Literal, Literal) (Literal, error)

func ValidateUnaryExpression(expr types.Type, operator operators.Operator) (types.Type, error) {
	if operator == operators.Dereference {
		if expr.Kind() != kinds.Pointer {
			return nil, fmt.Errorf("cannot dereference non-pointer type: %v", expr)
		}

		return types.Resolve(expr).(*types.Pointer).Pointee(), nil
	} else if operator == operators.Address {
		return types.NewPointer(expr), nil
	}

	kind := unaryOperatorKinds[UnaryOperatorKinds{Operator: operator, Operand: expr.Kind()}]
	if kind == kinds.Unknown {
		return nil, fmt.Errorf("invalid unary operator %q for kind %q", operator, expr.Kind())
	}

	if expr.Kind() != kind {
		return types.Kind(kind), nil
	}

	return expr, nil
}

var unaryOperatorKinds = map[UnaryOperatorKinds]kinds.Kind{
	//{operators.Dereference, kinds.Pointer}: kinds.Any,

	{operators.Address, kinds.Pointer}: kinds.Pointer,
	{operators.Address, kinds.Int}:     kinds.Pointer,
	{operators.Address, kinds.Float}:   kinds.Pointer,
	{operators.Address, kinds.String}:  kinds.Pointer,

	{operators.Negate, kinds.Int}:   kinds.Int,
	{operators.Negate, kinds.Float}: kinds.Float,
}

func ValidatePostfixExpression(left types.Type, operator operators.Operator) (types.Type, error) {
	op, err := operator.PostfixToInfix()
	if err != nil {
		return types.Unknown, err
	}

	kind := binaryOperatorKinds[BinaryOperatorKinds{op, left.Kind(), kinds.Int}]
	if kind == kinds.Unknown {
		return types.Unknown, fmt.Errorf("invalid postfix operator %q for type %q", operator, left)
	}

	return left, nil
}

type BinaryOperatorKinds struct {
	Operator    operators.Operator
	Left, Right kinds.Kind
}

type UnaryOperatorKinds struct {
	Operator operators.Operator
	Operand  kinds.Kind
}
