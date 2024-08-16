package compiler

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/parser"
)

type Function struct {
	name string
	pkg  *Package

	receiver   *Variable
	parameters []*Variable
	ret        Type
	body       []Statement

	symbols *SymbolScope

	bytecode BytecodeSnippet

	addr     Addr
	addrOp   Operand
	infoAddr Addr
}

func newFunction(name string, pkg *Package) *Function {
	f := &Function{
		name: name,
		pkg:  pkg,

		symbols: newScope(pkg.scope, name),

		addrOp: Operand{
			Kind: OperandKindImmediate,
		},
	}
	f.symbols.function = f

	return f
}

func (f *Function) Package() *Package {
	return f.pkg
}

func (f *Function) Name() string {
	if f.receiver.Type() == VoidType {
		return f.name
	}

	return fmt.Sprintf("%s.%s", f.receiver.Type(), f.name)
}

func (f *Function) QualifiedName() string {
	return fmt.Sprintf("%s.%s", f.pkg.QualifiedName(), f.Name())
}

func (f *Function) Type() Type {
	var paramTypes []Type
	for _, param := range f.parameters {
		paramTypes = append(paramTypes, param.Type())
	}

	var receiverTyp Type = VoidType
	if f.receiver != nil {
		receiverTyp = f.receiver.Type()
	}

	return &FunctionType{
		Receiver:   receiverTyp,
		Parameters: paramTypes,
		Return:     f.ret,
	}
}

func (f *Function) Receiver() *Variable {
	return f.receiver
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
	f.addrOp.Value = Int(f.addr)
}

func (f *Function) SetInfoAddr(addr Addr) {
	f.infoAddr = addr
}

func (f *Function) InfoAddr() Addr {
	return f.infoAddr
}

func (f *Function) AddrOp() *Operand {
	return &f.addrOp
}

func (f *Function) Bytecode() BytecodeSnippet {
	return f.bytecode
}

type CallExpression struct {
	Function Expression
	Args     []Expression

	parser.Position
}

func (e *CallExpression) Type() Type {
	switch ftype := e.Function.Type().(type) {
	case *FunctionType:
		return ftype.Return
	case *TypeType:
		return ftype.Type
	default:
		return UnknownType
	}
}

type ReturnStatement struct {
	Expression Expression

	Function *Function

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
		Receiver:   VoidType,
		Parameters: paramTypes,
		Return:     f.ret,
	}
}

const (
	ExternFuncKindInt Int = 0
	FuncKindInt       Int = 1
)

var externType = NewTupleType(
	IntType,
	StringType,
	NewPointerType(VoidType),
	StringType,
)

var funcType = NewTupleType(
	IntType,
	StringType,
	NewPointerType(VoidType),
	NewPointerType(VoidType),
)

type CompilerFunctionReferenceExpression struct {
	Function *Function
}

func (*CompilerFunctionReferenceExpression) Type() Type {
	return TypeKind(KindInt)
}

func (*CompilerFunctionReferenceExpression) WrapError(err error) error {
	return err
}

type BoundMethodExpression struct {
	Receiver Expression

	Method Method

	parser.Position
}

func (e *BoundMethodExpression) Type() Type {
	return e.Method.BoundFunctionType()
}

type MethodExpression struct {
	Receiver Expression

	Method Method

	parser.Position
}

func (e *MethodExpression) Type() Type {
	return &FunctionType{
		Receiver:   resolveType(e.Receiver.Type()),
		Parameters: e.Method.Parameters,
		Return:     e.Method.Return,
	}
}

type MethodFunctionExpression struct {
	Function *Function

	parser.Position
}

func (e *MethodFunctionExpression) Type() Type {
	return e.Function.Type().(*FunctionType).ToFunction()
}
