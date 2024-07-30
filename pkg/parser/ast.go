package parser

type Keyword string

const (
	KeywordFor     Keyword = "for"
	KeywordIf      Keyword = "if"
	KeywordElse    Keyword = "else"
	KeywordPackage Keyword = "package"
	KeywordFunc    Keyword = "func"
	KeywordMap     Keyword = "map"
	KeywordReturn  Keyword = "return"
)

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

type TypeDeclaration struct {
	Name Identifier
	Type Type
}

func (TypeDeclaration) declaration() {}

type Type interface {
	typ()
}

type PointerType struct {
	Pointee Type
}

func (PointerType) typ() {}

type SliceType struct {
	Element Type
}

func (SliceType) typ() {}

type TupleType struct {
	Elements []Type
}

func (TupleType) typ() {}

type MapType struct {
	Key   Type
	Value Type
}

func (MapType) typ() {}

func (Identifier) typ() {}

type FunctionDeclaration struct {
	Name       Identifier
	Parameters []Parameter
	Return     Type

	Body []Statement
}

func (FunctionDeclaration) declaration() {}

type VarDeclaration struct {
	Name Identifier
	Type *Type
	Expr *Expr
}

func (VarDeclaration) declaration() {}

type Parameter struct {
	Name *Identifier
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

type DeclarationStatement struct {
	Name Identifier
	Expr Expr
}

func (DeclarationStatement) statement() {}

type AssignmentStatement struct {
	Left  Expr
	Right Expr
}

func (AssignmentStatement) statement() {}

type AssignmentOperatorStatement struct {
	Left     Expr
	Operator Operator
	Right    Expr
}

func (AssignmentOperatorStatement) statement() {}

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
	Else      *ElseIfElseStatement
}

func (IfStatement) statement() {}

type ElseIfElseStatement interface {
	elseIfElseStatement()
}

type ElseIfStatement struct {
	Condition Expr
	Body      []Statement
	Else      *ElseIfElseStatement
}

func (ElseIfStatement) statement()           {}
func (ElseIfStatement) elseIfElseStatement() {}

type ElseStatement struct {
	Body []Statement
}

func (ElseStatement) statement()           {}
func (ElseStatement) elseIfElseStatement() {}

type ForStatement struct {
	Init      *Statement
	Condition *Expr
	Step      *Statement

	Body []Statement
}

type ForRangeStatement struct {
}

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

func (UnaryExpr) expr() {}
