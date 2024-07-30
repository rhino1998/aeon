package parser

type Identifier string

type Program struct {
	Package Package

	Declarations []Declaration
}

type Package struct {
	Name Identifier
}

type Declaration interface {
	declaration()
}

type Type interface {
	typ()
}

type PointerType struct {
	Pointee Type
}

func (PointerType) typ() {}

func (Identifier) typ() {}

type FunctionDeclaration struct {
	Name       Identifier
	Parameters []Parameter
	Return     *Type

	Body []Statement
}

func (FunctionDeclaration) declaration() {}

type Parameter struct {
	Name Identifier
	Type Type
}

type Statement interface {
	statement()
}

type VarStatement struct {
	Name Identifier
	Type *Type
	Expr *Expr
}

func (VarStatement) statement() {}

type ExprStatement struct {
	Expr Expr
}

func (ExprStatement) statement() {}

type PostfixStatement struct {
	Expr     Expr
	Operator Operator
}

func (PostfixStatement) statement() {}

type IfStatement struct {
	Condition Expr
	Body      []Statement
	Else      ElseIfElseStatement
}

func (IfStatement) statement() {}

type ElseIfElseStatement interface {
	elseIfElseStatement()
}

type ElseIfStatement struct {
	Condition Expr
	Body      []Statement
	Else      ElseIfElseStatement
}

func (ElseIfStatement) statement()           {}
func (ElseIfStatement) elseIfElseStatement() {}

type ElseStatement struct {
	Body []Statement
}

func (ElseStatement) statement()           {}
func (ElseStatement) elseIfElseStatement() {}

type Expr interface {
	expr()
}

type NumberLiteral float64

func (NumberLiteral) expr() {}

func (l NumberLiteral) IsInteger() bool {
	return float64(int64(l)) == float64(l)
}

type StringLiteral string

func (StringLiteral) expr() {}

type BooleanLiteral bool

func (BooleanLiteral) expr() {}

type CallExpr struct {
	Expr Expr
	Args []Expr
}

func (CallExpr) expr() {}

type IdentifierExpr struct {
	Identifier Identifier
}

func (IdentifierExpr) expr() {}

type DotExpr struct {
	Expr Expr
	Key  Identifier
}

func (DotExpr) expr() {}

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
)

type BinaryExpr struct {
	Left     Expr
	Operator Operator
	Right    Expr
}

func (BinaryExpr) expr() {}

type UnaryExpr struct {
	Operator Operator
	Expr     Expr
}
