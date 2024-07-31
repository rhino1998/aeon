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
	kind := binaryOperatorKinds[operator][[2]Kind{left.Kind(), right.Kind()}]
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

var binaryOperatorKinds = map[Operator]map[[2]Kind]Kind{
	OperatorPower: binaryOperatorNumericKinds,
	OperatorAddition: {
		{KindFloat, KindFloat}:   KindFloat,
		{KindInt, KindInt}:       KindInt,
		{KindString, KindString}: KindString,
	},
	OperatorSubtraction:    binaryOperatorNumericKinds,
	OperatorMultiplication: binaryOperatorNumericKinds,
	OperatorDivision:       binaryOperatorNumericKinds,
	OperatorModulo:         binaryOperatorNumericKinds,

	OperatorPlusEquals: {
		{KindFloat, KindFloat}:   KindFloat,
		{KindInt, KindInt}:       KindInt,
		{KindString, KindString}: KindString,
	},
	OperatorMinusEquals:    binaryOperatorNumericKinds,
	OperatorMultiplyEquals: binaryOperatorNumericKinds,
	OperatorDivideEquals:   binaryOperatorNumericKinds,
	OperatorModuloEquals:   binaryOperatorNumericKinds,

	OperatorBitwiseAnd: binaryOperatorIntegerKinds,
	OperatorBitwiseOr:  binaryOperatorIntegerKinds,
	OperatorBitwiseXor: binaryOperatorIntegerKinds,
	OperatorLeftShift:  binaryOperatorIntegerKinds,
	OperatorRightShift: binaryOperatorIntegerKinds,

	OperatorLogicalOr:  binaryOperatorBooleanKinds,
	OperatorLogicalAnd: binaryOperatorBooleanKinds,

	OperatorGreaterThanOrEqual: binaryOperatorComparisonKinds,
	OperatorGreaterThan:        binaryOperatorComparisonKinds,
	OperatorLessThan:           binaryOperatorComparisonKinds,
	OperatorLessThanOrEqual:    binaryOperatorComparisonKinds,
	OperatorEqual:              binaryOperatorComparisonKinds,
	OperatorNotEqual:           binaryOperatorComparisonKinds,
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
	expr     Expression
	operator Operator

	typ Type

	parser.Position
}

func (e *UnaryExpression) Type() Type {
	return e.typ
}
