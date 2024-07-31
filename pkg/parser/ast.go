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

	Position Position
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

	Position Position
}

func (TypeDeclaration) declaration() {}

type Type interface {
	typ()
}

type PointerType struct {
	Pointee Type

	Position Position
}

func (PointerType) typ() {}

type SliceType struct {
	Element Type

	Position Position
}

func (SliceType) typ() {}

type TupleType struct {
	Elements []Type

	Position Position
}

func (TupleType) typ() {}

type MapType struct {
	Key   Type
	Value Type

	Position Position
}

func (MapType) typ() {}

func (Identifier) typ() {}

type FunctionDeclaration struct {
	Name       Identifier
	Parameters []Parameter
	Return     Type

	Body []Statement

	Position Position
}

func (FunctionDeclaration) declaration() {}

type VarDeclaration struct {
	Name Identifier
	Type *Type
	Expr *Expr

	Position Position
}

func (VarDeclaration) declaration() {}

type Parameter struct {
	Name *Identifier
	Type Type

	Position Position
}

type Statement interface {
	statement()
}

type VarStatement struct {
	Name Identifier
	Type *Type
	Expr *Expr

	Position Position
}

func (VarStatement) statement() {}

type DeclarationStatement struct {
	Name Identifier
	Expr Expr

	Position Position
}

func (DeclarationStatement) statement() {}

type AssignmentStatement struct {
	Left  Expr
	Right Expr

	Position Position
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

	Position Position
}

func (ExprStatement) statement() {}

type PostfixStatement struct {
	Expr     Expr
	Operator Operator

	Position Position
}

func (PostfixStatement) statement() {}

type IfStatement struct {
	Condition Expr
	Body      []Statement
	Else      ElseIfElseStatement

	Position Position
}

func (IfStatement) statement() {}

type ElseIfElseStatement interface {
	elseIfElseStatement()
}

type ElseIfStatement struct {
	Condition Expr
	Body      []Statement
	Else      ElseIfElseStatement

	Position Position
}

func (ElseIfStatement) statement()           {}
func (ElseIfStatement) elseIfElseStatement() {}

type ElseStatement struct {
	Body []Statement

	Position Position
}

func (ElseStatement) statement()           {}
func (ElseStatement) elseIfElseStatement() {}

type ForStatement struct {
	Init      Statement
	Condition Expr
	Step      Statement

	Body []Statement

	Position Position
}

type ForRangeStatement struct {
}

type ReturnStatement struct {
	Expr Expr

	Position Position
}

func (ReturnStatement) statement() {}

type Expr interface {
	expr()
}

type NumberLiteral struct {
	Value float64

	Position Position
}

func (NumberLiteral) expr() {}

func (l NumberLiteral) IsInteger() bool {
	return float64(int64(l.Value)) == float64(l.Value)
}

type StringLiteral struct {
	Value string

	Position Position
}

func (StringLiteral) expr() {}

type BooleanLiteral struct {
	Value bool

	Position Position
}

func (BooleanLiteral) expr() {}

type CallExpr struct {
	Expr Expr
	Args []Expr

	Position Position
}

func (CallExpr) expr() {}

type IdentifierExpr struct {
	Identifier Identifier

	Position Position
}

func (IdentifierExpr) expr() {}

type DotExpr struct {
	Expr Expr
	Key  Identifier

	Position Position
}

func (DotExpr) expr() {}

type Operator string

type BinaryExpr struct {
	Left     Expr
	Operator Operator
	Right    Expr

	Position Position
}

func (BinaryExpr) expr() {}

type UnaryExpr struct {
	Operator Operator
	Expr     Expr

	Position Position
}

func (UnaryExpr) expr() {}
