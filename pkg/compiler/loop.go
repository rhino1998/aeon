package compiler

import "github.com/rhino1998/aeon/pkg/parser"

type ForStatement struct {
	Init      Statement
	Condition Expression
	Step      Statement

	Scope *Scope
	Body  []Statement

	parser.Position
}
