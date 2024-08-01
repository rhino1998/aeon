package compiler

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/parser"
)

type Operator string

const (
	OperatorPower Operator = "**"

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
)

var binaryOperatorNumericKinds = map[[2]Kind]Kind{
	{KindFloat, KindFloat}: KindFloat,
	{KindInt, KindInt}:     KindInt,
}

var binaryOperatorComparisonKinds = map[[2]Kind]Kind{
	{KindFloat, KindFloat}:   KindBool,
	{KindInt, KindInt}:       KindBool,
	{KindString, KindString}: KindBool,
	{KindBool, KindBool}:     KindBool,
}

var binaryOperatorIntegerKinds = map[[2]Kind]Kind{
	{KindInt, KindInt}: KindInt,
}

var binaryOperatorBooleanKinds = map[[2]Kind]Kind{
	{KindBool, KindBool}: KindBool,
}

func validateBinaryExpression(left Type, operator Operator, right Type) (Type, error) {
	kind := binaryOperatorKinds[BinaryOperatorKinds{operator, left.Kind(), right.Kind()}]
	if kind == KindUnknown {
		return nil, fmt.Errorf("invalid binary operator %q for kinds %q and %q", operator, left.Kind(), right.Kind())
	}

	if !TypesEqual(left, right) {
		return nil, fmt.Errorf("invalid binary operator %q for types %q and %q", operator, left, right)
	}

	if left.Kind() != kind {
		return KindType(kind), nil
	}

	return left, nil
}

var binaryOperatorKinds = map[BinaryOperatorKinds]Kind{
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
	{OperatorModulo, KindInt, KindInt}:     KindInt,
	{OperatorModulo, KindFloat, KindFloat}: KindFloat,

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

var unaryOperatorNumericKinds = map[Kind]Kind{
	KindInt:   KindInt,
	KindFloat: KindFloat,
}

var unaryOperatorIntegerKinds = map[Kind]Kind{
	KindInt: KindInt,
}

var unaryOperatorBooleanKinds = map[Kind]Kind{
	KindBool: KindBool,
}

func validateUnaryExpression(expr Type, operator Operator) (Type, error) {
	kind := unaryOperatorKinds[operator][expr.Kind()]
	if kind == KindUnknown {
		return nil, fmt.Errorf("invalid unary operator %q for kind %q", operator, expr.Kind())
	}

	if expr.Kind() != kind {
		return UnspecifiedKindType{kind: kind}, nil
	}

	return expr, nil
}

var unaryOperatorKinds = map[Operator]map[Kind]Kind{
	OperatorNegate:    unaryOperatorNumericKinds,
	OperatorPositive:  unaryOperatorNumericKinds,
	OperatorIncrement: unaryOperatorNumericKinds,
	OperatorDecrement: unaryOperatorNumericKinds,

	OperatorBitwiseNot: unaryOperatorIntegerKinds,

	OperatorNot: unaryOperatorBooleanKinds,
}

type BinaryExpression struct {
	Left     Expression
	Operator Operator
	Right    Expression

	typ Type

	parser.Position
}

func (e *BinaryExpression) Type() Type {
	return e.typ
}

func (e *BinaryExpression) SetType(typ Type) {
	e.typ = typ
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
	typ        Type

	parser.Position
}

func (e *UnaryExpression) Type() Type {
	// TODO: resolve
	return e.typ
}

type BinaryOperatorKinds struct {
	Operator    Operator
	Left, Right Kind
}
