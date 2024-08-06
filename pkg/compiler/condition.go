package compiler

import "github.com/rhino1998/aeon/pkg/parser"

type IfStatement struct {
	Condition Expression

	Scope *SymbolScope
	Body  []Statement

	Else Statement

	parser.Position
}
