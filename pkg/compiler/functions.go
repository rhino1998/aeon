package compiler

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler/air"
	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/operators"
	"github.com/rhino1998/aeon/pkg/compiler/types"
	"github.com/rhino1998/aeon/pkg/parser"
)

type Function struct {
	name string
	pkg  *Package

	receiver   *Variable
	parameters []*Variable
	ret        types.Type
	body       []Statement

	parser.Position

	symbols *SymbolScope

	bytecode air.Snippet

	addr       air.Addr
	addrOp     air.Operand
	infoAddrOp *air.Operand

	locals []types.Type
}

type TypeSlot struct {
	Offset air.Size
	Type   types.Type
}

func newFunction(name string, pkg *Package) *Function {
	f := &Function{
		name: name,
		pkg:  pkg,

		symbols: newScope(pkg.scope, name),

		addrOp: *air.IntOperand(0),
	}
	f.symbols.function = f

	return f
}

func (f *Function) withPointerReceiver() *Function {
	symbols := newScope(f.symbols.parent, f.name)
	ptrReceiver := &Variable{
		name: "#recv",
		typ:  types.NewPointer(f.receiver.typ),
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
		addrOp:  *air.IntOperand(0),

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
						Operator: operators.Dereference,
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

func (f *Function) StackLayout() ([]TypeSlot, error) {
	frameSize := f.symbols.Package().prog.FrameSize()

	layout := make([]TypeSlot, 0, len(f.parameters)+2+int(frameSize))

	fStackSize, err := air.FunctionStackSize(f.Type().(*types.Function))
	if err != nil {
		return nil, err
	}

	offset := -(fStackSize + frameSize) + 1

	addLayout := func(typ types.Type) error {
		typSize, err := air.TypeSize(typ)
		if err != nil {
			return err
		}

		if typSize == 0 {
			return nil
		}

		layout = append(layout, TypeSlot{Offset: offset, Type: typ})
		offset += typSize

		return nil
	}

	addLayout(f.ret)
	addLayout(f.receiver.typ)

	for _, param := range f.parameters {
		addLayout(param.typ)
	}

	for range frameSize {
		addLayout(TypeInt)
	}

	for _, local := range f.locals {
		addLayout(local)
	}

	return layout, nil
}

func (f *Function) Package() *Package {
	return f.pkg
}

func (f *Function) Name() string {
	if f.receiver.Type() == types.Void {
		return f.name
	}

	return fmt.Sprintf("%s.%s", f.receiver.Type(), f.name)
}

func (f *Function) QualifiedName() string {
	return fmt.Sprintf("%s.%s", f.pkg.QualifiedName(), f.Name())
}

func (f *Function) Type() types.Type {
	var paramTypes []types.Type
	for _, param := range f.parameters {
		if param.variadic {
			paramTypes = append(paramTypes, param.Type().(*types.Slice).AsVariadic())
		} else {
			paramTypes = append(paramTypes, param.Type())
		}
	}

	return &types.Function{
		Receiver:   f.receiver.Type(),
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

func (f *Function) Return() types.Type {
	return f.ret
}

func (f *Function) Body() []Statement {
	return f.body
}

func (f *Function) Addr() air.Addr {
	return f.addr
}

func (f *Function) AddrRange() (air.Addr, air.Addr) {
	return f.addr, f.addr + air.Addr(len(f.bytecode))
}

func (f *Function) OffsetAddr(addr air.Addr) {
	f.addr += addr
	f.addrOp.Value = f.addrOp.Value.(air.Int) + air.Int(addr)
}

func (f *Function) SetInfoAddr(op *air.Operand) {
	f.infoAddrOp = op
}

func (f *Function) InfoAddr() air.Addr {
	return air.Addr(f.infoAddrOp.Value.(air.Int))
}

func (f *Function) AddrOp() *air.Operand {
	return &f.addrOp
}

func (f *Function) Instructions() air.Snippet {
	return f.bytecode
}

type CallExpression struct {
	Function Expression
	Args     []Expression

	parser.Position
}

func (e *CallExpression) Type() types.Type {
	switch ftype := e.Function.Type().(type) {
	case *types.Function:
		return ftype.Return
	case *types.TypeType:
		return ftype.Type
	default:
		return types.Unknown
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
	ret        types.Type
}

func NewExternFunction(name string, params []*Variable, ret types.Type) *ExternFunction {
	return &ExternFunction{
		name:       name,
		parameters: params,
		ret:        ret,
	}
}

func (f *ExternFunction) FlatParameterKinds() ([]kinds.Kind, error) {
	var ret []kinds.Kind
	for _, param := range f.parameters {
		flatParam, err := f.flattenParameters(param.Type())
		if err != nil {
			return nil, err
		}

		ret = append(ret, flatParam...)
	}

	return ret, nil
}

func (f *ExternFunction) flattenParameters(param types.Type) ([]kinds.Kind, error) {
	param = types.Base(param)
	kind := param.Kind()
	switch kind {
	case kinds.Unknown:
		return nil, fmt.Errorf("cannot pass unknown type to extern function")
	case kinds.Void:
		return nil, nil
	case kinds.Nil:
		return nil, fmt.Errorf("cannot pass nil type to extern function")
	case kinds.Int, kinds.Float, kinds.Bool, kinds.String, kinds.Pointer, kinds.Type:
		return []kinds.Kind{kind}, nil
	case kinds.Array:
		var ret []kinds.Kind
		array := param.(*types.Array)

		for range array.Length() {
			flatElem, err := f.flattenParameters(array.Elem())
			if err != nil {
				return nil, err
			}

			ret = append(ret, flatElem...)
		}
		return ret, nil
	case kinds.Tuple:
		var ret []kinds.Kind
		tuple := param.(*types.Tuple)
		for _, elem := range tuple.Elems() {
			flatElem, err := f.flattenParameters(elem)
			if err != nil {
				return nil, err
			}

			ret = append(ret, flatElem...)
		}
		return ret, nil
	case kinds.Struct:
		var ret []kinds.Kind
		struc := param.(*types.Struct)
		for _, field := range struc.Fields() {
			flatField, err := f.flattenParameters(field.Type)
			if err != nil {
				return nil, err
			}

			ret = append(ret, flatField...)
		}
		return ret, nil
	case kinds.Interface:
		return []kinds.Kind{kinds.Type, kinds.Pointer}, nil
	case kinds.Function:
		return nil, fmt.Errorf("cannot pass function type %s to extern function", param)
	case kinds.Map:
		return nil, fmt.Errorf("cannot pass map type %s to extern function (because it is unimplemented)", param)
	case kinds.Slice:
		return []kinds.Kind{kinds.Pointer, kinds.Int, kinds.Int}, nil
	default:
		return nil, fmt.Errorf("cannot pass unhandled type %s to extern function", param)
	}
}

func (f *ExternFunction) Name() string {
	return f.name
}

func (f *ExternFunction) Type() types.Type {
	var paramTypes []types.Type
	for _, param := range f.parameters {
		if param.variadic {
			paramTypes = append(paramTypes, param.Type().(*types.Slice).AsVariadic())
		} else {
			paramTypes = append(paramTypes, param.Type())
		}
	}

	return &types.Function{
		Receiver:   types.Void,
		Parameters: paramTypes,
		Return:     f.ret,
	}
}

type CompilerFunctionReferenceExpression struct {
	Function *Function
}

func (*CompilerFunctionReferenceExpression) Type() types.Type {
	return types.Kind(kinds.Int)
}

func (*CompilerFunctionReferenceExpression) WrapError(err error) error {
	return err
}

type BoundMethodExpression struct {
	Receiver Expression

	Method types.Method

	parser.Position
}

func (e *BoundMethodExpression) Type() types.Type {
	return e.Method.BoundFunction()
}

type MethodExpression struct {
	Receiver Expression

	Method types.Method

	parser.Position
}

func (e *MethodExpression) Type() types.Type {
	return &types.Function{
		Receiver:   types.Dereference(e.Receiver.Type()),
		Parameters: e.Method.Parameters,
		Return:     e.Method.Return,
	}
}

type MethodFunctionExpression struct {
	Function *Function

	parser.Position
}

func (e *MethodFunctionExpression) Type() types.Type {
	return e.Function.Type().(*types.Function).ToFunction()
}
