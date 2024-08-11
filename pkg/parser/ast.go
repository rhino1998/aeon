package parser

import "fmt"

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

type baseType struct{}

func (baseType) typ() {}

type baseExpr struct{}

func (baseExpr) expr() {}

type baseStatement struct{}

func (baseStatement) statement() {}

type baseDeclaration struct{}

func (baseDeclaration) declaration() {}

type Position struct {
	File   string
	Line   int
	Column int
}

type PositionError struct {
	Err error
	Position
}

func (e PositionError) Unwrap() error {
	return e.Err
}

func (e PositionError) Error() string {
	return fmt.Sprintf("%s:%d:%d: %v", e.File, e.Line, e.Column, e.Err)

}

func (p Position) WrapError(err error) error {
	return PositionError{
		Err:      err,
		Position: p,
	}
}

func pos(c *current) Position {
	return Position{Line: c.pos.line, Column: c.pos.col, File: c.state["filename"].(string)}
}

type Identifier string

func (Identifier) typ() {}

type File struct {
	Package Package

	Declarations []Declaration

	Position
}

type Package struct {
	Name Identifier
}

type Declaration interface {
	declaration()
	WrapError(error) error
}

type TypeDeclaration struct {
	baseDeclaration

	Name Identifier
	Type Type

	Position
}

type Type interface {
	typ()
}

type PointerType struct {
	baseType

	Pointee Type

	Position
}

type SliceType struct {
	baseType

	Element Type

	Position
}

type TupleType struct {
	baseType

	Elements []Type

	Position
}

type MapType struct {
	baseType

	Key   Type
	Value Type

	Position
}

type FunctionDeclaration struct {
	baseDeclaration

	Name       Identifier
	Parameters []Parameter
	Return     Type

	Body []Statement

	Position
}

type ExternFunctionDeclaration struct {
	baseDeclaration

	Name       Identifier
	Parameters []Parameter
	Return     Type

	Position
}

type VarDeclaration struct {
	baseDeclaration

	Name Identifier
	Type *Type
	Expr *Expr

	Position
}

type ConstDeclaration struct {
	baseDeclaration

	Name Identifier
	Type *Type
	Expr *Expr

	Position
}

type Parameter struct {
	Name *Identifier
	Type Type

	Position
}

type Statement interface {
	statement()

	WrapError(err error) error
}

type VarStatement struct {
	baseStatement

	Name Identifier
	Type *Type
	Expr *Expr

	Position
}

type DeclarationStatement struct {
	baseStatement

	Name Identifier
	Expr Expr

	Position
}

type AssignmentStatement struct {
	baseStatement

	Left  Expr
	Right Expr

	Position
}

type AssignmentOperatorStatement struct {
	baseStatement

	Left     Expr
	Operator Operator
	Right    Expr

	Position
}

type ExprStatement struct {
	baseStatement

	Expr Expr

	Position
}

type PostfixStatement struct {
	baseStatement

	Expr     Expr
	Operator Operator

	Position
}

type IfStatement struct {
	baseStatement

	Condition Expr
	Body      []Statement
	Else      ElseIfElseStatement

	Position
}

type ElseIfElseStatement interface {
	Statement

	elseIfElseStatement()
}

type baseElseIfElseStatement struct {
	baseStatement
}

func (baseElseIfElseStatement) elseIfElseStatement() {}

type ElseIfStatement struct {
	baseElseIfElseStatement

	Condition Expr
	Body      []Statement
	Else      ElseIfElseStatement

	Position
}

type ElseStatement struct {
	baseElseIfElseStatement

	Body []Statement

	Position
}

type ForStatement struct {
	baseStatement

	Init      Statement
	Condition Expr
	Step      Statement

	Body []Statement

	Position
}

type ForRangeStatement struct {
}

type ReturnStatement struct {
	baseStatement

	Expr Expr

	Position
}

func (ReturnStatement) statement() {}

type Expr interface {
	expr()

	WrapError(err error) error
}

type FloatLiteral struct {
	baseExpr

	Value float64

	Position
}

type IntLiteral struct {
	baseExpr

	Value int64

	Position
}

type StringLiteral struct {
	baseExpr

	Value string

	Position
}

type BoolLiteral struct {
	baseExpr

	Value bool

	Position
}

type ParenthesesExpr struct {
	baseExpr

	Expr Expr

	Position
}

type CallExpr struct {
	baseExpr

	Expr Expr
	Args []Expr

	Position
}

type IdentifierExpr struct {
	baseExpr

	Identifier Identifier

	Position
}

type DotExpr struct {
	baseExpr

	Expr Expr
	Key  Identifier

	Position
}

type Operator string

type BinaryExpr struct {
	baseExpr

	Left     Expr
	Operator Operator
	Right    Expr

	Position
}

type UnaryExpr struct {
	baseExpr

	Operator Operator
	Expr     Expr

	Position
}

type ParenthesizedExpr struct {
	baseExpr

	Expr Expr

	Position
}

type Directive struct {
	Name        Identifier
	Args        []any
	Declaration Declaration

	Position
}

func (d Directive) declaration() {}
