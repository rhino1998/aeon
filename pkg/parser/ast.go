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
	Text   string
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

func (e Position) String() string {
	return e.Text
}

func (e Position) RawText() string {
	return e.Text
}

func (p Position) WrapError(err error) error {
	return PositionError{
		Err:      err,
		Position: p,
	}
}

func pos(c *current) Position {
	return Position{Line: c.pos.line, Column: c.pos.col, File: c.state["filename"].(string), Text: string(c.text)}
}

type Identifier struct {
	Str string

	Position
}

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

	WrapError(error) error
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

type ArrayType struct {
	baseType

	Length  IntLiteral
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

type FunctionType struct {
	baseType

	Parameters []Parameter
	Return     Type

	Position
}

type MethodDeclaration struct {
	baseDeclaration

	Receiver   Parameter
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

	Names []Identifier
	Expr  Expr

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

type ParenthesizedType struct {
	baseType

	Type Type

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

type IndexExpr struct {
	baseExpr

	Expr  Expr
	Index Expr

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
	baseDeclaration

	Name        Identifier
	Args        []any
	Declaration Declaration

	Position
}

type TupleExpr struct {
	baseExpr

	Elems []Expr

	Position
}

type ArrayExpr struct {
	baseExpr

	Length IntLiteral
	Type   Type
	Elems  []Expr

	Position
}

type StructType struct {
	baseType

	Fields []StructField

	Position
}

type StructField struct {
	Name Identifier
	Type Type

	Position
}

type InterfaceType struct {
	baseType

	Methods []InterfaceMethod

	Position
}

type InterfaceMethod struct {
	Name       Identifier
	Parameters []Parameter
	Return     Type

	Position
}

type TypeExpr struct {
	baseExpr

	Type Type

	Position
}

type VariadicType struct {
	baseType

	Type Type

	Position
}

type SpreadExpr struct {
	baseExpr

	Expr Expr

	Position
}
