package compiler

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/parser"
)

type Operator string

func (o Operator) CanOperand() bool {
	switch o {
	case OperatorAddition,
		OperatorSubtraction,
		OperatorBoundsCheck,
		OperatorLessThan,
		OperatorLessThanOrEqual,
		OperatorEqual,
		OperatorNotEqual,
		OperatorGreaterThan,
		OperatorGreaterThanOrEqual:
		return true
	case OperatorNot:
		//OperatorNegate,
		//OperatorPositive
		return true
	default:
		return false
	}
}

func (o Operator) IsVectorizable() bool {
	switch o {
	case OperatorAddition,
		OperatorSubtraction,
		//OperatorNegate,
		//OperatorPositive
		OperatorMultiplication,
		OperatorDivision,
		OperatorModulo,
		OperatorExponentiation,
		OperatorPlusEquals,
		OperatorMinusEquals,
		OperatorMultiplyEquals,
		OperatorDivideEquals,
		OperatorModuloEquals,
		OperatorExponentiationEquals,
		OperatorBitwiseAnd,
		OperatorBitwiseOr,
		OperatorBitwiseXor,
		//OperatorBitwiseNot,
		OperatorEqual,
		OperatorNotEqual,
		OperatorGreaterThan,
		OperatorLessThan,
		OperatorGreaterThanOrEqual,
		OperatorLessThanOrEqual:
		return true
	default:
		return false
	}
}

func (o Operator) AssignmentToInfix() (Operator, error) {
	switch o {
	case OperatorPlusEquals:
		return OperatorAddition, nil
	case OperatorMinusEquals:
		return OperatorSubtraction, nil
	case OperatorMultiplyEquals:
		return OperatorMultiplication, nil
	case OperatorDivideEquals:
		return OperatorDivision, nil
	case OperatorModuloEquals:
		return OperatorModulo, nil
	case OperatorExponentiationEquals:
		return OperatorExponentiation, nil
	default:
		return "", fmt.Errorf("operator %q is not an assignment operator", o)
	}
}

func (o Operator) PostfixToInfix() (Operator, error) {
	switch o {
	case OperatorIncrement:
		return OperatorAddition, nil
	case OperatorDecrement:
		return OperatorSubtraction, nil
	default:
		return "", fmt.Errorf("operator %q is not a postfix operator", o)
	}
}

const (
	OperatorExponentiation Operator = "**"

	OperatorMultiplication Operator = "*"
	OperatorDivision       Operator = "/"
	OperatorModulo         Operator = "%"
	OperatorLeftShift      Operator = "<<"
	OperatorRightShift     Operator = ">>"
	OperatorBitwiseAnd     Operator = "&"

	OperatorAddition    Operator = "+"
	OperatorSubtraction Operator = "-"
	OperatorBitwiseOr   Operator = "|"
	OperatorBitwiseXor  Operator = "^"

	OperatorEqual              Operator = "=="
	OperatorNotEqual           Operator = "!="
	OperatorLessThan           Operator = "<"
	OperatorGreaterThan        Operator = ">"
	OperatorLessThanOrEqual    Operator = "<="
	OperatorGreaterThanOrEqual Operator = ">="

	OperatorLogicalAnd Operator = "&&"

	OperatorLogicalOr Operator = "||"

	OperatorIncrement Operator = "++"
	OperatorDecrement Operator = "--"

	OperatorNegate     Operator = "-"
	OperatorBitwiseNot Operator = "^"
	OperatorPositive   Operator = "+"
	OperatorNot        Operator = "!"

	OperatorPlusEquals           Operator = "+="
	OperatorMinusEquals          Operator = "-="
	OperatorMultiplyEquals       Operator = "*="
	OperatorDivideEquals         Operator = "/="
	OperatorModuloEquals         Operator = "%="
	OperatorExponentiationEquals Operator = "**="

	OperatorAddress     Operator = "&"
	OperatorDereference Operator = "*"

	OperatorBoundsCheck Operator = "#"
)

func validateBinaryExpression(left Type, operator Operator, right Type) (Type, error) {
	kind := binaryOperatorKinds[BinaryOperatorKinds{operator, left.Kind(), right.Kind()}]
	if kind == KindUnknown {
		return UnknownType, fmt.Errorf("invalid binary operator %q for types %q and %q", operator, left, right)
	}

	if !TypesEqual(left, right) {
		return TypeKind(kind), fmt.Errorf("invalid binary operator %q for types %q and %q", operator, left, right)
	}

	if left.Kind() != kind {
		return TypeKind(kind), nil
	}

	return left, nil
}

var binaryOperatorKinds = map[BinaryOperatorKinds]Kind{
	// **
	{OperatorExponentiation, KindInt, KindInt}:     KindInt,
	{OperatorExponentiation, KindFloat, KindFloat}: KindFloat,

	// +
	{OperatorAddition, KindInt, KindInt}:       KindInt,
	{OperatorAddition, KindFloat, KindFloat}:   KindFloat,
	{OperatorAddition, KindString, KindString}: KindString,

	// -
	{OperatorSubtraction, KindInt, KindInt}:     KindInt,
	{OperatorSubtraction, KindFloat, KindFloat}: KindFloat,

	// *
	{OperatorMultiplication, KindInt, KindInt}:     KindInt,
	{OperatorMultiplication, KindFloat, KindFloat}: KindFloat,

	// /
	{OperatorDivision, KindInt, KindInt}:     KindInt,
	{OperatorDivision, KindFloat, KindFloat}: KindFloat,

	// %
	{OperatorModulo, KindInt, KindInt}: KindInt,

	// ==
	{OperatorEqual, KindInt, KindInt}:         KindBool,
	{OperatorEqual, KindFloat, KindFloat}:     KindBool,
	{OperatorEqual, KindString, KindString}:   KindBool,
	{OperatorEqual, KindBool, KindBool}:       KindBool,
	{OperatorEqual, KindPointer, KindPointer}: KindBool,

	// !=
	{OperatorNotEqual, KindInt, KindInt}:         KindBool,
	{OperatorNotEqual, KindFloat, KindFloat}:     KindBool,
	{OperatorNotEqual, KindString, KindString}:   KindBool,
	{OperatorNotEqual, KindBool, KindBool}:       KindBool,
	{OperatorNotEqual, KindPointer, KindPointer}: KindBool,

	// <
	{OperatorLessThan, KindInt, KindInt}:     KindBool,
	{OperatorLessThan, KindFloat, KindFloat}: KindBool,

	// <=
	{OperatorLessThanOrEqual, KindInt, KindInt}:     KindBool,
	{OperatorLessThanOrEqual, KindFloat, KindFloat}: KindBool,

	// >
	{OperatorGreaterThan, KindInt, KindInt}:     KindBool,
	{OperatorGreaterThan, KindFloat, KindFloat}: KindBool,

	// >=
	{OperatorGreaterThanOrEqual, KindInt, KindInt}:     KindBool,
	{OperatorGreaterThanOrEqual, KindFloat, KindFloat}: KindBool,
}

var binaryOperatorEvaluators = map[BinaryOperatorKinds]binaryOperatorEvaluatorFunc{
	{OperatorAddition, KindInt, KindInt}:       binaryOperatorArithmeticEvaluator[Int](binaryOperatorAdd),
	{OperatorAddition, KindFloat, KindFloat}:   binaryOperatorArithmeticEvaluator[Float](binaryOperatorAdd),
	{OperatorAddition, KindString, KindString}: binaryOperatorArithmeticEvaluator[String](binaryOperatorAdd),

	{OperatorSubtraction, KindInt, KindInt}:     binaryOperatorArithmeticEvaluator[Int](binaryOperatorSub),
	{OperatorSubtraction, KindFloat, KindFloat}: binaryOperatorArithmeticEvaluator[Float](binaryOperatorSub),

	{OperatorMultiplication, KindInt, KindInt}:     binaryOperatorArithmeticEvaluator[Int](binaryOperatorMult),
	{OperatorMultiplication, KindFloat, KindFloat}: binaryOperatorArithmeticEvaluator[Float](binaryOperatorMult),

	{OperatorDivision, KindInt, KindInt}:     binaryOperatorArithmeticEvaluator[Int](binaryOperatorDiv),
	{OperatorDivision, KindFloat, KindFloat}: binaryOperatorArithmeticEvaluator[Float](binaryOperatorDiv),
}

func evaluateBinaryOperator(left LiteralValue, op Operator, right LiteralValue) (LiteralValue, error) {
	fun, ok := binaryOperatorEvaluators[BinaryOperatorKinds{op, left.Kind(), right.Kind()}]
	if !ok {
		return nil, fmt.Errorf("invalid binary operator %q for types %q and %q", op, left.Kind(), right.Kind())
	}

	return fun(left, right)
}

func binaryOperatorArithmeticEvaluator[T LiteralValue](f func(T, T) (T, error)) binaryOperatorEvaluatorFunc {
	return func(a, b LiteralValue) (LiteralValue, error) {
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

type binaryOperatorEvaluatorFunc func(LiteralValue, LiteralValue) (LiteralValue, error)

func validateUnaryExpression(expr Type, operator Operator) (Type, error) {
	if operator == OperatorDereference {
		if expr.Kind() != KindPointer {
			return nil, fmt.Errorf("cannot dereference non-pointer type: %v", expr)
		}

		return dereferenceType(expr).(*PointerType).Pointee(), nil
	} else if operator == OperatorAddress {
		return NewPointerType(expr), nil
	}

	kind := unaryOperatorKinds[UnaryOperatorKinds{Operator: operator, Operand: expr.Kind()}]
	if kind == KindUnknown {
		return nil, fmt.Errorf("invalid unary operator %q for kind %q", operator, expr.Kind())
	}

	if expr.Kind() != kind {
		return TypeKind(kind), nil
	}

	return expr, nil
}

var unaryOperatorKinds = map[UnaryOperatorKinds]Kind{
	//{OperatorDereference, KindPointer}: KindAny,

	{OperatorAddress, KindPointer}: KindPointer,
	{OperatorAddress, KindInt}:     KindPointer,
	{OperatorAddress, KindFloat}:   KindPointer,
	{OperatorAddress, KindString}:  KindPointer,

	{OperatorNegate, KindInt}:   KindInt,
	{OperatorNegate, KindFloat}: KindFloat,
}

func validatePostfixExpression(left Type, operator Operator) (Type, error) {
	op, err := operator.PostfixToInfix()
	if err != nil {
		return UnknownType, err
	}

	kind := binaryOperatorKinds[BinaryOperatorKinds{op, left.Kind(), KindInt}]
	if kind == KindUnknown {
		return UnknownType, fmt.Errorf("invalid postfix operator %q for type %q", operator, left)
	}

	return left, nil
}

type BinaryExpression struct {
	Left     Expression
	Operator Operator
	Right    Expression

	parser.Position
}

func (e *BinaryExpression) Evaluate() (LiteralValue, error) {
	_, err := validateBinaryExpression(e.Left.Type(), e.Operator, e.Right.Type())
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

	return evaluateBinaryOperator(left, e.Operator, right)
}

func (e *BinaryExpression) Type() Type {
	switch e.Operator {
	case OperatorEqual, OperatorNotEqual, OperatorLessThan, OperatorLessThanOrEqual, OperatorGreaterThan, OperatorGreaterThanOrEqual:
		return TypeKind(KindBool)
	default:
		typ, err := validateBinaryExpression(e.Left.Type(), e.Operator, e.Right.Type())
		if err != nil {
			return typ
		}

		return typ
	}
}

type AssignmentOperatorStatement struct {
	Left     Expression
	Operator Operator
	Right    Expression

	parser.Position
}

type UnaryExpression struct {
	Expression Expression
	Operator   Operator

	parser.Position
}

func (e *UnaryExpression) Type() Type {
	typ, err := validateUnaryExpression(e.Expression.Type(), e.Operator)
	if err != nil {
		return UnknownType
	}

	return typ
}

type BinaryOperatorKinds struct {
	Operator    Operator
	Left, Right Kind
}

type UnaryOperatorKinds struct {
	Operator Operator
	Operand  Kind
}

type Operation string

func BinaryOperation(left Kind, op Operator, right Kind) Operation {
	return Operation(fmt.Sprintf("%s%s%s", shortKind(left), string(op), shortKind(right)))
}

func UnaryOperation(op Operator, operand Kind) Operation {
	return Operation(fmt.Sprintf("%s%s", string(op), shortKind(operand)))
}
