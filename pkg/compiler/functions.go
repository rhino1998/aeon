package compiler

import "github.com/rhino1998/aeon/pkg/parser"

type Function struct {
	name string
	pkg  *Package

	parameters []*Variable
	ret        Type
	body       []Statement

	symbols *SymbolScope

	bytecode BytecodeSnippet

	addr   Addr
	addrOp Operand
}

func (f *Function) Package() *Package {
	return f.pkg
}

func (f *Function) Name() string {
	return f.name
}

func (f *Function) Type() Type {
	var paramTypes []Type
	for _, param := range f.parameters {
		paramTypes = append(paramTypes, param.Type())
	}

	return &FunctionType{
		Parameters: paramTypes,
		Return:     f.ret,
	}
}

func (f *Function) Parameters() []*Variable {
	return f.parameters
}

func (f *Function) Return() Type {
	return f.ret
}

func (f *Function) Body() []Statement {
	return f.body
}

func (f *Function) Addr() Addr {
	return f.addr
}

func (f *Function) SetAddr(addr Addr) {
	f.addr = addr
	f.addrOp.Value = f.addr
}

func (f *Function) AddrOp() *Operand {
	return &f.addrOp
}

type CallExpression struct {
	Function Expression
	Args     []Expression

	parser.Position
}

func (e *CallExpression) Type() Type {
	return BaseType(e.Function.Type()).(*FunctionType).Return
}

type ReturnStatement struct {
	Expression Expression

	parser.Position
}

type ExternFunction struct {
	name string

	parameters []*Variable
	ret        Type
}

func (f *ExternFunction) Name() string {
	return f.name
}

func (f *ExternFunction) Type() Type {
	var paramTypes []Type
	for _, param := range f.parameters {
		paramTypes = append(paramTypes, param.Type())
	}

	return &FunctionType{
		Parameters: paramTypes,
		Return:     f.ret,
	}
}

const (
	ExternFuncKindInt Int = 0
	FuncKindInt       Int = 1
)

var externType = TupleType{
	elems: []Type{
		IntType,
		StringType,
		IntType,
		StringType,
	},
}

var funcType = TupleType{
	elems: []Type{
		IntType,
		StringType,
		IntType,
		KindType(KindPointer),
	},
}

var closureType = TupleType{
	elems: []Type{
		IntType,
		StringType,
		IntType, // currently unused
		KindType(KindPointer),
	},
}
