package compiler

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/parser"
)

type Operator string

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

	OperatorPlusEquals     Operator = "+="
	OperatorMinusEquals    Operator = "-="
	OperatorMultiplyEquals Operator = "*="
	OperatorDivideEquals   Operator = "/="
	OperatorModuloEquals   Operator = "%="

	OperatorAddress     Operator = "&"
	OperatorDereference Operator = "*"

	OperatorBoundsCheck Operator = "#"
)

func validateBinaryExpression(left Type, operator Operator, right Type) (Type, error) {
	kind := binaryOperatorKinds[BinaryOperatorKinds{operator, left.Kind(), right.Kind()}]
	if kind == KindUnknown {
		return nil, fmt.Errorf("invalid binary operator %q for types %q and %q", operator, left, right)
	}

	if !TypesEqual(left, right) {
		return nil, fmt.Errorf("invalid binary operator %q for types %q and %q", operator, left, right)
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
	{OperatorAddition, KindInt, KindInt}:         KindInt,
	{OperatorAddition, KindFloat, KindFloat}:     KindFloat,
	{OperatorAddition, KindString, KindString}:   KindString,
	{OperatorPlusEquals, KindInt, KindInt}:       KindInt,
	{OperatorPlusEquals, KindFloat, KindFloat}:   KindFloat,
	{OperatorPlusEquals, KindString, KindString}: KindString,

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
	{OperatorEqual, KindInt, KindInt}:       KindBool,
	{OperatorEqual, KindFloat, KindFloat}:   KindBool,
	{OperatorEqual, KindString, KindString}: KindBool,
	{OperatorEqual, KindBool, KindBool}:     KindBool,

	// !=
	{OperatorNotEqual, KindInt, KindInt}:       KindBool,
	{OperatorNotEqual, KindFloat, KindFloat}:   KindBool,
	{OperatorNotEqual, KindString, KindString}: KindBool,
	{OperatorNotEqual, KindBool, KindBool}:     KindBool,

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

func validateUnaryExpression(expr Type, operator Operator) (Type, error) {
	if operator == OperatorDereference {
		if expr.Kind() != KindPointer {
			return nil, fmt.Errorf("cannot dereference non-pointer type: %v", expr)
		}

		return BaseType(expr).(*PointerType).Pointee(), nil
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

type BinaryExpression struct {
	Left     Expression
	Operator Operator
	Right    Expression

	parser.Position
}

func (e *BinaryExpression) Type() Type {
	typ, err := validateBinaryExpression(e.Left.Type(), e.Operator, e.Right.Type())
	if err != nil {
		return UnknownType
	}

	return typ
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
