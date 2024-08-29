package compiler

import (
	"fmt"
	"strings"

	"github.com/rhino1998/aeon/pkg/parser"
)

type Function struct {
	name string
	pkg  *Package

	receiver   *Variable
	parameters []*Variable
	ret        Type
	body       []Statement

	parser.Position

	symbols *SymbolScope

	bytecode BytecodeSnippet

	addr     Addr
	addrOp   Operand
	infoAddr Addr

	stackLayout []Type
}

func newFunction(name string, pkg *Package) *Function {
	f := &Function{
		name: name,
		pkg:  pkg,

		symbols: newScope(pkg.scope, name),

		addrOp: Operand{
			Kind:  OperandKindImmediate,
			Value: Int(0),
		},
	}
	f.symbols.function = f

	return f
}

func (f *Function) withPointerReceiver() *Function {
	symbols := newScope(f.symbols.parent, f.name)
	ptrReceiver := &Variable{
		name: "#recv",
		typ:  NewPointerType(f.receiver.typ),
	}
	symbols.put(ptrReceiver)

	var newParams []*Variable
	var newArgs []Expression
	for i, param := range f.parameters {
		paramName := fmt.Sprintf("#arg_%d", i)
		newParam := &Variable{
			name: paramName,
			typ:  param.typ,
		}
		newParams = append(newParams, newParam)
		symbols.put(newParam)
		newArgs = append(newArgs, &SymbolReferenceExpression{
			scope: symbols,
			name:  paramName,
		})
	}

	ptrF := &Function{
		name: f.name,
		pkg:  f.pkg,

		symbols: symbols,
		addrOp: Operand{
			Kind:  OperandKindImmediate,
			Value: Int(0),
		},

		receiver:   ptrReceiver,
		parameters: newParams,
		ret:        f.ret,

		Position: f.Position,
	}

	ptrF.body = append(ptrF.body,
		&ReturnStatement{
			Function: ptrF,
			Expression: &CallExpression{
				Function: &DotExpression{
					Receiver: &UnaryExpression{
						Operator: OperatorDereference,
						Expression: &SymbolReferenceExpression{
							scope: symbols,
							name:  ptrReceiver.name,

							Position: f.Position,
						},
					},
					Key:      f.name,
					Position: f.Position,
				},
				Args:     newArgs,
				Position: f.Position,
			},
			Position: f.Position,
		},
	)

	return ptrF
}

func (f *Function) StackLayout() []Type {
	layout := make([]Type, 0, len(f.parameters)+2+3)

	layout = append(layout, f.ret)
	layout = append(layout, f.receiver.typ)
	for _, param := range f.parameters {
		layout = append(layout, param.typ)
	}

	layout = append(layout, TypeInt, TypeInt, TypeInt)

	layout = append(layout, f.stackLayout...)
	return layout
}

func (f *Function) Package() *Package {
	return f.pkg
}

func (f *Function) Name() string {
	if f.receiver.Type() == TypeVoid {
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
		if param.variadic {
			paramTypes = append(paramTypes, param.Type().(*SliceType).AsVariadic())
		} else {
			paramTypes = append(paramTypes, param.Type())
		}
	}

	var receiverTyp Type = TypeVoid
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

func (f *Function) AddrRange() (Addr, Addr) {
	return f.addr, f.addr + Addr(len(f.bytecode))
}

func (f *Function) OffsetAddr(addr Addr) {
	f.addr += addr
	f.addrOp.Value = f.addrOp.Value.(Int) + Int(addr)
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

func (f *Function) ReturnArgSize() Size {
	var offset Size
	offset += f.Return().Size()
	offset += f.Receiver().Type().Size()
	for _, param := range f.Parameters() {
		offset += param.Type().Size()
	}

	return offset
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

func NewExternFunction(name string, params []*Variable, ret Type) *ExternFunction {
	return &ExternFunction{
		name:       name,
		parameters: params,
		ret:        ret,
	}
}

func (f *ExternFunction) FlatParameterKinds() ([]Kind, error) {
	var ret []Kind
	for _, param := range f.parameters {
		flatParam, err := f.flattenParameters(param.Type())
		if err != nil {
			return nil, err
		}

		ret = append(ret, flatParam...)
	}

	return ret, nil
}

func (f *ExternFunction) flattenParameters(param Type) ([]Kind, error) {
	param = baseType(param)
	kind := param.Kind()
	switch kind {
	case KindUnknown:
		return nil, fmt.Errorf("cannot pass unknown type to extern function")
	case KindVoid:
		return nil, nil
	case KindNil:
		return nil, fmt.Errorf("cannot pass nil type to extern function")
	case KindInt, KindFloat, KindBool, KindString, KindPointer, KindType:
		return []Kind{kind}, nil
	case KindArray:
		var ret []Kind
		array := param.(*ArrayType)
		length := array.Length()
		if length == nil {
			return nil, fmt.Errorf("cannot pass array of unknown length to extern function")
		}

		for range *length {
			flatElem, err := f.flattenParameters(array.Elem())
			if err != nil {
				return nil, err
			}

			ret = append(ret, flatElem...)
		}
		return ret, nil
	case KindTuple:
		var ret []Kind
		tuple := param.(*TupleType)
		for _, elem := range tuple.Elems() {
			flatElem, err := f.flattenParameters(elem)
			if err != nil {
				return nil, err
			}

			ret = append(ret, flatElem...)
		}
		return ret, nil
	case KindStruct:
		var ret []Kind
		struc := param.(*StructType)
		for _, field := range struc.Fields() {
			flatField, err := f.flattenParameters(field.Type)
			if err != nil {
				return nil, err
			}

			ret = append(ret, flatField...)
		}
		return ret, nil
	case KindInterface:
		return []Kind{KindType, KindPointer}, nil
	case KindFunction:
		return nil, fmt.Errorf("cannot pass function type %s to extern function", param)
	case KindMap:
		return nil, fmt.Errorf("cannot pass map type %s to extern function (because it is unimplemented)", param)
	case KindSlice:
		return []Kind{KindPointer, KindInt, KindInt}, nil
	default:
		return nil, fmt.Errorf("cannot pass unhandled type %s to extern function", param)
	}
}

func (f *ExternFunction) Name() string {
	return f.name
}

func (f *ExternFunction) Type() Type {
	var paramTypes []Type
	for _, param := range f.parameters {
		if param.variadic {
			paramTypes = append(paramTypes, param.Type().(*SliceType).AsVariadic())
		} else {
			paramTypes = append(paramTypes, param.Type())
		}
	}

	return &FunctionType{
		Receiver:   TypeVoid,
		Parameters: paramTypes,
		Return:     f.ret,
	}
}

const (
	ExternFuncKindInt Int = 0
	FuncKindInt       Int = 1
)

var externType = NewTupleType(
	TypeInt,
	TypeString,
	TypeString,
	NewPointerType(TypeVoid),
	TypeString,
)

var funcType = NewTupleType(
	TypeInt,
	TypeString,
	TypeString,
	NewPointerType(TypeVoid),
	NewPointerType(TypeVoid),
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
		Receiver:   dereferenceType(e.Receiver.Type()),
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

type FunctionType struct {
	Receiver   Type
	Parameters []Type
	Return     Type
}

func NewFunctionType(recv Type, parameters []Type, ret Type) *FunctionType {
	return &FunctionType{
		Receiver:   recv,
		Parameters: parameters,
		Return:     ret,
	}
}

func (t *FunctionType) ToFunction() *FunctionType {
	if t.Receiver == TypeVoid {
		return t
	}

	return &FunctionType{
		Receiver:   TypeVoid,
		Parameters: append([]Type{t.Receiver}, t.Parameters...),
		Return:     TypeVoid,
	}
}

func (t *FunctionType) MethodEqual(o *FunctionType) bool {
	if len(t.Parameters) != len(o.Parameters) {
		return false
	}

	for i := range len(t.Parameters) {
		if !TypesEqual(t.Parameters[i], o.Parameters[i]) {
			return false
		}
	}

	return TypesEqual(t.Return, o.Return)
}

func (t *FunctionType) Kind() Kind { return KindFunction }

func (t *FunctionType) String() string {
	params := make([]string, 0, len(t.Parameters))
	for _, param := range t.Parameters {
		params = append(params, string(param.GlobalName()))
	}

	var recvStr string
	if t.Receiver != TypeVoid {
		recvStr = fmt.Sprintf("(%s) ", t.Receiver)
	}

	var retStr string
	if t.Return != TypeVoid {
		retStr = fmt.Sprintf(" %s", t.Return)
	}

	return fmt.Sprintf("%sfunc(%s)%s", recvStr, strings.Join(params, ", "), retStr)
}

func (t *FunctionType) GlobalName() TypeName {
	params := make([]string, 0, len(t.Parameters))
	for _, param := range t.Parameters {
		params = append(params, string(param.GlobalName()))
	}

	var recvStr string
	if t.Receiver != TypeVoid {
		recvStr = fmt.Sprintf("(%s) ", string(t.Receiver.GlobalName()))
	}

	var retStr string
	if t.Return != TypeVoid {
		retStr = fmt.Sprintf(" %s", string(t.Return.GlobalName()))
	}

	return TypeName(fmt.Sprintf("%sfunc(%s)%s", recvStr, strings.Join(params, ", "), retStr))
}

func (FunctionType) Size() Size {
	return 1
}
