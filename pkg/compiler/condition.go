package compiler

import "github.com/rhino1998/aeon/pkg/parser"

type IfStatement struct {
	Condition Expression

	Scope *Scope
	Body  []Statement

	Else Statement

	parser.Position
}

type ElseIfStatement struct {
	Condition Expression

	Scope *Scope
	Body  []Statement

	Else Statement

	parser.Position
}

type ElseStatement struct {
	Scope *Scope
	Body  []Statement

	parser.Position
}
