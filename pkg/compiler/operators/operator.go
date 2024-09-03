package operators

import (
	"fmt"
)

type Operator string

func (o Operator) CanOperand() bool {
	switch o {
	case Addition,
		Subtraction,
		BoundsCheck,
		LessThan,
		LessThanOrEqual,
		Equal,
		NotEqual,
		GreaterThan,
		GreaterThanOrEqual:
		return true
	case Not:
		//Negate,
		//Positive
		return true
	default:
		return false
	}
}

func (o Operator) IsComparison() bool {
	switch o {
	case Equal,
		NotEqual,
		LessThan,
		GreaterThan,
		LessThanOrEqual,
		GreaterThanOrEqual:
		return true
	default:
		return false
	}
}

func (o Operator) IsVectorizable() bool {
	switch o {
	case Addition,
		Subtraction,
		//Negate,
		//Positive
		Multiplication,
		Division,
		Modulo,
		Exponentiation,
		PlusEquals,
		MinusEquals,
		MultiplyEquals,
		DivideEquals,
		ModuloEquals,
		ExponentiationEquals,
		BitwiseAnd,
		BitwiseOr,
		BitwiseXor,
		//BitwiseNot,
		Equal,
		NotEqual,
		GreaterThan,
		LessThan,
		GreaterThanOrEqual,
		LessThanOrEqual:
		return true
	default:
		return false
	}
}

func (o Operator) AssignmentToInfix() (Operator, error) {
	switch o {
	case PlusEquals:
		return Addition, nil
	case MinusEquals:
		return Subtraction, nil
	case MultiplyEquals:
		return Multiplication, nil
	case DivideEquals:
		return Division, nil
	case ModuloEquals:
		return Modulo, nil
	case ExponentiationEquals:
		return Exponentiation, nil
	default:
		return "", fmt.Errorf("operator %q is not an assignment operator", o)
	}
}

func (o Operator) PostfixToInfix() (Operator, error) {
	switch o {
	case Increment:
		return Addition, nil
	case Decrement:
		return Subtraction, nil
	default:
		return "", fmt.Errorf("operator %q is not a postfix operator", o)
	}
}

const (
	Exponentiation Operator = "**"

	Multiplication Operator = "*"
	Division       Operator = "/"
	Modulo         Operator = "%"
	LeftShift      Operator = "<<"
	RightShift     Operator = ">>"

	Addition    Operator = "+"
	Subtraction Operator = "-"

	BitwiseOr  Operator = "|"
	BitwiseXor Operator = "^"
	BitwiseAnd Operator = "&"

	Equal              Operator = "=="
	NotEqual           Operator = "!="
	LessThan           Operator = "<"
	GreaterThan        Operator = ">"
	LessThanOrEqual    Operator = "<="
	GreaterThanOrEqual Operator = ">="

	LogicalAnd Operator = "&&"

	LogicalOr Operator = "||"

	Increment Operator = "++"
	Decrement Operator = "--"

	Negate     Operator = "-"
	BitwiseNot Operator = "^"
	Positive   Operator = "+"
	Not        Operator = "!"

	PlusEquals           Operator = "+="
	MinusEquals          Operator = "-="
	MultiplyEquals       Operator = "*="
	DivideEquals         Operator = "/="
	ModuloEquals         Operator = "%="
	ExponentiationEquals Operator = "**="

	Address     Operator = "&"
	Dereference Operator = "*"

	BoundsCheck Operator = "#"
)
