package compiler

import "fmt"

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
	{KindFloat, KindFloat}:     KindFloat,
	{KindInteger, KindInteger}: KindInteger,
}

var binaryOperatorComparisonKinds = map[[2]Kind]Kind{
	{KindFloat, KindFloat}:     KindBoolean,
	{KindInteger, KindInteger}: KindBoolean,
	{KindString, KindString}:   KindBoolean,
	{KindBoolean, KindBoolean}: KindBoolean,
}

var binaryOperatorIntegerKinds = map[[2]Kind]Kind{
	{KindInteger, KindInteger}: KindInteger,
}

var binaryOperatorBooleanKinds = map[[2]Kind]Kind{
	{KindBoolean, KindBoolean}: KindBoolean,
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
		return UnspecifiedKindType{kind: kind}, nil
	}

	return left, nil
}

var binaryOperatorKinds = map[Operator]map[[2]Kind]Kind{
	OperatorPower: binaryOperatorNumericKinds,
	OperatorAddition: {
		{KindFloat, KindFloat}:     KindFloat,
		{KindInteger, KindInteger}: KindInteger,
		{KindString, KindString}:   KindString,
	},
	OperatorSubtraction:    binaryOperatorNumericKinds,
	OperatorMultiplication: binaryOperatorNumericKinds,
	OperatorDivision:       binaryOperatorNumericKinds,
	OperatorModulo:         binaryOperatorNumericKinds,

	OperatorPlusEquals:     binaryOperatorNumericKinds,
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
	KindInteger: KindInteger,
	KindFloat:   KindFloat,
}

var unaryOperatorIntegerKinds = map[Kind]Kind{
	KindInteger: KindInteger,
}

var unaryOperatorBooleanKinds = map[Kind]Kind{
	KindBoolean: KindBoolean,
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
	left     Expression
	operator Operator
	right    Expression

	typ Type
}

func (e *BinaryExpression) Type() Type {
	return e.typ
}

type AssignmentOperatorStatement struct {
	left     Expression
	operator Operator
	right    Expression
}
