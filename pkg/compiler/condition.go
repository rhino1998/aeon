package compiler

import "github.com/rhino1998/aeon/pkg/parser"

type IfStatement struct {
	condition Expression

	body []Statement

	els Statement

	parser.Position
}

func (s *IfStatement) Condition() Expression {
	return s.condition
}

func (s *IfStatement) Body() []Statement {
	return s.body
}

func (s *IfStatement) Else() Statement {
	return s.els
}
