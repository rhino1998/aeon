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
