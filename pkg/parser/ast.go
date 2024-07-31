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

type Program struct {
	Package Package

	Declarations []Declaration

	Position
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

	Position
}

func (TypeDeclaration) declaration() {}

type Type interface {
	typ()
}

type PointerType struct {
	Pointee Type

	Position
}

func (PointerType) typ() {}

type SliceType struct {
	Element Type

	Position
}

func (SliceType) typ() {}

type TupleType struct {
	Elements []Type

	Position
}

func (TupleType) typ() {}

type MapType struct {
	Key   Type
	Value Type

	Position
}

func (MapType) typ() {}

func (Identifier) typ() {}

type FunctionDeclaration struct {
	Name       Identifier
	Parameters []Parameter
	Return     Type

	Body []Statement

	Position
}

func (FunctionDeclaration) declaration() {}

type VarDeclaration struct {
	Name Identifier
	Type *Type
	Expr *Expr

	Position
}

func (VarDeclaration) declaration() {}

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
	Name Identifier
	Type *Type
	Expr *Expr

	Position
}

func (VarStatement) statement() {}

type DeclarationStatement struct {
	Name Identifier
	Expr Expr

	Position
}

func (DeclarationStatement) statement() {}

type AssignmentStatement struct {
	Left  Expr
	Right Expr

	Position
}

func (AssignmentStatement) statement() {}

type AssignmentOperatorStatement struct {
	Left     Expr
	Operator Operator
	Right    Expr

	Position
}

func (AssignmentOperatorStatement) statement() {}

type ExprStatement struct {
	Expr Expr

	Position
}

func (ExprStatement) statement() {}

type PostfixStatement struct {
	Expr     Expr
	Operator Operator

	Position
}

func (PostfixStatement) statement() {}

type IfStatement struct {
	Condition Expr
	Body      []Statement
	Else      ElseIfElseStatement

	Position
}

func (IfStatement) statement() {}

type ElseIfElseStatement interface {
	elseIfElseStatement()
}

type ElseIfStatement struct {
	Condition Expr
	Body      []Statement
	Else      ElseIfElseStatement

	Position
}

func (ElseIfStatement) statement()           {}
func (ElseIfStatement) elseIfElseStatement() {}

type ElseStatement struct {
	Body []Statement

	Position
}

func (ElseStatement) statement()           {}
func (ElseStatement) elseIfElseStatement() {}

type ForStatement struct {
	Init      Statement
	Condition Expr
	Step      Statement

	Body []Statement

	Position
}

type ForRangeStatement struct {
}

type ReturnStatement struct {
	Expr Expr

	Position
}

func (ReturnStatement) statement() {}

type Expr interface {
	expr()

	WrapError(err error) error
}

type NumberLiteral struct {
	Value float64

	Position
}

func (NumberLiteral) expr() {}

func (l NumberLiteral) IsInteger() bool {
	return float64(int64(l.Value)) == float64(l.Value)
}

type StringLiteral struct {
	Value string

	Position
}

func (StringLiteral) expr() {}

type BooleanLiteral struct {
	Value bool

	Position
}

func (BooleanLiteral) expr() {}

type CallExpr struct {
	Expr Expr
	Args []Expr

	Position
}

func (CallExpr) expr() {}

type IdentifierExpr struct {
	Identifier Identifier

	Position
}

func (IdentifierExpr) expr() {}

type DotExpr struct {
	Expr Expr
	Key  Identifier

	Position
}

func (DotExpr) expr() {}

type Operator string

type BinaryExpr struct {
	Left     Expr
	Operator Operator
	Right    Expr

	Position
}

func (BinaryExpr) expr() {}

type UnaryExpr struct {
	Operator Operator
	Expr     Expr

	Position
}

func (UnaryExpr) expr() {}
