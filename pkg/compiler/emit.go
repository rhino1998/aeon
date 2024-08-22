package compiler

import (
	"context"
	"fmt"
	"strconv"

	"github.com/rhino1998/aeon/pkg/parser"
)

func (c *Compiler) compileBC(ctx context.Context, prog *Program) error {
	// TODO: sort by import orderings
	registerOperands := make([]*Operand, prog.Registers())

	for reg := range Register(prog.Registers()) {
		if reg < 3 {
			continue
		}
		registerOperands[reg] = ImmediateOperand(Int(0))
		prog.bytecode.Add(Mov{
			Src:  registerOperands[reg],
			Dst:  RegisterOperand(reg),
			Size: 1,
		})
	}

	for _, pkg := range prog.Packages() {
		err := c.compileBCPackage(ctx, pkg)
		if err != nil {
			return err
		}

		prog.bytecode.Mount(pkg)

		for _, typ := range pkg.KnownTypes() {
			name := typ.GlobalName()
			if other, ok := prog.types[name]; ok {
				if !TypesEqual(typ, other) {
					panic(fmt.Errorf("non-equal duplicate types %s %s", typ, other))
				}
			} else {
				prog.types[name] = typ
			}
		}

		for _, str := range pkg.Strings() {
			prog.strings[str] = struct{}{}
		}
	}

	err := prog.bytecode.ResolveLabels()
	if err != nil {
		return err
	}

	err = prog.bytecode.ResolveTypes(prog.Types())
	if err != nil {
		return err
	}

	err = prog.bytecode.ResolveStrings(prog.Strings())
	if err != nil {
		return err
	}

	prog.bytecode.OptimizeOperands()

	// for i, bc := range prog.bytecode {
	// 	log.Printf("%02x: %v", i, bc)
	// }

	return nil
}

func (c *Compiler) compileBCPackage(ctx context.Context, pkg *Package) error {
	pkg.values = BuiltinValues(pkg.prog.Registers(), pkg.scope)

	// TODO: sort topologically
	for _, constant := range pkg.Constants() {
		val, err := constant.Evaluate()
		if err != nil {
			return err
		}

		pkg.values.newConstant(constant.Name(), constant.Type(), val.Location(pkg.values).Operand)
	}

	varInitFunc, err := c.compilePackageVarInit(ctx, pkg)
	if err != nil {
		return err
	}

	pkg.bytecode.Mount(varInitFunc)

	for _, fun := range pkg.Functions() {
		err := c.compileBCFunction(ctx, fun, pkg.values)
		if err != nil {
			return err
		}

		pkg.bytecode.Mount(fun)
	}

	for _, drv := range pkg.DerivedTypes() {
		for _, met := range drv.MethodFunctions() {
			err := c.compileBCFunction(ctx, met, pkg.values)
			if err != nil {
				return err
			}

			pkg.bytecode.Mount(met)
		}

		for _, met := range drv.PtrMethodFunctions() {
			err := c.compileBCFunction(ctx, met, pkg.values)
			if err != nil {
				return err
			}

			pkg.bytecode.Mount(met)
		}
	}

	err = pkg.scope.put(varInitFunc)
	if err != nil {
		return err
	}

	return nil
}

func (c *Compiler) compilePackageVarInit(ctx context.Context, pkg *Package) (*Function, error) {
	f := newFunction(VarInitFuncName, pkg)
	f.receiver = &Variable{typ: TypeVoid}

	scope := pkg.values
	// TODO: special varinit tmp var semantics

	numLocals := scope.newImmediate(Int(0))

	f.bytecode.Mov(scope.SP(), scope.SP().Add(numLocals))

	// TODO: make these truly global
	for _, externFunc := range pkg.prog.root.ExternFunctions() {
		hdr := scope.newGlobal("@"+externFunc.Name(), externType)
		hdrBC, err := c.compileBCValuesLiteral(ctx, pkg.prog, []Expression{
			NewLiteral(Int(1)),
			NewLiteral(String(externFunc.Name())),
			NewLiteral(Int(0)),
			NewLiteral(String(externFunc.Name())),
		}, scope, hdr)
		if err != nil {
			return nil, err
		}

		f.bytecode.Add(hdrBC...)

		scope.newConstant(externFunc.Name(), externFunc.Type(), hdr.Operand.AddressOf())
	}

	for _, externFunc := range pkg.ExternFunctions() {
		hdr := scope.newGlobal("@"+externFunc.Name(), externType)
		hdrBC, err := c.compileBCValuesLiteral(ctx, pkg.prog, []Expression{
			NewLiteral(Int(1)),
			NewLiteral(String(externFunc.Name())),
			NewLiteral(Int(0)),
			NewLiteral(String(externFunc.Name())),
		}, scope, hdr)
		if err != nil {
			return nil, err
		}

		f.bytecode.Add(hdrBC...)

		scope.newConstant(externFunc.Name(), externFunc.Type(), hdr.Operand.AddressOf())
	}

	for _, fun := range pkg.Functions() {
		fName := fun.Name()
		hdr := scope.newGlobal("@"+fName, funcType)
		hdrBC, err := c.compileBCValuesLiteral(ctx, pkg.prog, []Expression{
			NewLiteral(Int(2)),
			NewLiteral(String(fun.pkg.qualifiedName)),
			NewLiteral(Int(0)),
			&CompilerFunctionReferenceExpression{fun},
		}, scope, hdr)
		if err != nil {
			return nil, err
		}

		f.bytecode.Add(hdrBC...)

		scope.newConstant(fName, fun.Type(), hdr.Operand.AddressOf())

		fun.SetInfoAddr(Addr(hdr.AddressOf().Value.(Int)))
	}

	for _, global := range pkg.Globals() {
		ptr := scope.newGlobal(global.Name(), global.Type())
		if global.expr != nil {
			exprBC, exprLoc, err := c.compileBCExpression(ctx, pkg.prog, global.expr, scope, ptr)
			if err != nil {
				return nil, err
			}

			f.bytecode.Add(exprBC...)

			if exprLoc != ptr {
				f.bytecode.Add(Mov{
					Src:  exprLoc.Operand,
					Dst:  ptr.Operand,
					Size: global.Type().Size(),
				})
			}
		} else {
			exprBC, exprLoc, err := c.compileBCZeroValue(ctx, pkg.prog, global.Position, global.Type(), scope, ptr)
			if err != nil {
				return nil, err
			}

			f.bytecode.Add(exprBC...)

			if exprLoc != ptr {
				f.bytecode.Add(scope.Mov(exprLoc, ptr))
			}
		}
	}

	for _, drv := range pkg.DerivedTypes() {
		for _, met := range drv.MethodFunctions() {
			hdr := scope.newGlobal("@"+met.Name(), funcType)
			hdrBC, err := c.compileBCValuesLiteral(ctx, pkg.prog, []Expression{
				NewLiteral(Int(2)),
				NewLiteral(String(met.QualifiedName())),
				NewLiteral(Int(0)),
				&CompilerFunctionReferenceExpression{met},
			}, scope, hdr)
			if err != nil {
				return nil, err
			}

			f.bytecode.Add(hdrBC...)

			scope.newConstant(met.Name(), met.Type(), hdr.Operand.AddressOf())

			met.SetInfoAddr(Addr(hdr.AddressOf().Value.(Int)))
		}

		for _, met := range drv.PtrMethodFunctions() {
			hdr := scope.newGlobal("@"+met.Name(), funcType)
			hdrBC, err := c.compileBCValuesLiteral(ctx, pkg.prog, []Expression{
				NewLiteral(Int(2)),
				NewLiteral(String(met.QualifiedName())),
				NewLiteral(Int(0)),
				&CompilerFunctionReferenceExpression{met},
			}, scope, hdr)
			if err != nil {
				return nil, err
			}

			f.bytecode.Add(hdrBC...)

			scope.newConstant(met.Name(), met.Type(), hdr.Operand.AddressOf())

			met.SetInfoAddr(Addr(hdr.AddressOf().Value.(Int)))
		}
	}

	f.bytecode.Add(Ret{})

	numLocals.Value = Int(*scope.maxLocal)

	pkg.varinit = f

	return f, nil
}

func (c *Compiler) compileBCFunction(ctx context.Context, f *Function, scope *ValueScope) error {
	scope = scope.fun(f.symbols)

	numLocals := scope.newImmediate(Int(0))
	f.bytecode.Mov(scope.SP(), scope.SP().Add(numLocals))

	var argReturnSize Size
	argReturnSize += f.Return().Size()
	argReturnSize += f.Receiver().Type().Size()
	for _, param := range f.Parameters() {
		argReturnSize += param.Type().Size()
	}

	baseOffset := -(f.pkg.prog.FrameSize() + argReturnSize)

	var offset Size = 1
	scope.newParam("__return", baseOffset+offset, f.Return())
	offset += f.Return().Size()

	if f.Receiver().Name() != "" {
		scope.newParam(f.Receiver().Name(), baseOffset+offset, f.Receiver().Type())
	}
	offset += f.Receiver().Type().Size()

	for _, param := range f.Parameters() {
		if param.Name() != "" {
			scope.newParam(param.Name(), baseOffset+offset, param.Type())
		}

		offset += param.Type().Size()
	}

	body := f.Body()

	for _, stmt := range body {
		bcs, err := c.compileBCStatement(ctx, f.pkg.prog, stmt, scope)
		if err != nil {
			return err
		}

		f.bytecode.Add(bcs...)
	}

	var last Statement
	if len(body) > 0 {
		last = body[len(body)-1]
	}

	// implicit return at end of void functions
	if _, ok := last.(*ReturnStatement); !ok && f.Return() == TypeVoid {
		f.bytecode.Add(Ret{Args: argReturnSize})
	}

	// Can resolve here for debugging
	// err := f.bytecode.ResolveLabels()
	// if err != nil {
	// 	return err
	// }

	numLocals.Value = Int(*scope.maxLocal)

	return nil
}

func (c *Compiler) compileBCGlobal(ctx context.Context, prog *Program, name string, expr Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, error) {
	var bc BytecodeSnippet
	globBC, globLoc, err := c.compileBCExpression(ctx, prog, expr, scope, dst)
	if err != nil {
		return nil, err
	}

	bc.Add(globBC...)

	if globLoc != dst {
		bc.Add(scope.Mov(globLoc, dst))
	}

	return bc, nil
}

func (c *Compiler) compileBCLHS(ctx context.Context, prog *Program, expr Expression, scope *ValueScope) (BytecodeSnippet, *Location, error) {
	var bc BytecodeSnippet
	switch expr := expr.(type) {
	case *SymbolReferenceExpression:
		op, ok := scope.Get(expr.Name())
		if !ok {
			return nil, nil, expr.WrapError(fmt.Errorf("cannot assign to unknown symbol %q", expr.Name()))
		}
		return nil, op, nil
	case *ParenthesizedExpression:
		return c.compileBCLHS(ctx, prog, expr, scope)
	case *DotExpression:
		receiverTmp := scope.allocTemp(expr.Receiver.Type())
		defer scope.deallocTemp(receiverTmp)
		receiverBC, receiverLoc, err := c.compileBCExpression(ctx, prog, expr.Receiver, scope, receiverTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(receiverBC...)

		lhsBC, lhsLoc, err := c.compileBCLHSDotExpression(ctx, prog, expr, expr.Receiver.Type(), scope, receiverLoc)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(lhsBC...)

		return bc, lhsLoc, nil
	case *IndexExpression:
		receiverTmp := scope.allocTemp(expr.Receiver.Type())
		defer scope.deallocTemp(receiverTmp)
		receiverBC, receiverLoc, err := c.compileBCExpression(ctx, prog, expr.Receiver, scope, receiverTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(receiverBC...)

		indexTmp := scope.allocTemp(expr.Index.Type())
		defer scope.deallocTemp(indexTmp)
		indexBC, indexLoc, err := c.compileBCExpression(ctx, prog, expr.Index, scope, indexTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(indexBC...)

		lhsBC, lhsLoc, err := c.compileBCLHSIndexExpression(ctx, prog, expr, expr.Receiver.Type(), scope, receiverLoc, indexLoc)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(lhsBC...)

		return bc, lhsLoc, nil
	case *UnaryExpression:
		switch expr.Operator {
		case OperatorDereference:
			tmp := scope.allocTemp(expr.Expression.Type())
			defer scope.deallocTemp(tmp)
			subExprBC, subExprLoc, err := c.compileBCExpression(ctx, prog, expr.Expression, scope, tmp)
			if err != nil {
				return nil, nil, err
			}

			bc.Add(subExprBC...)

			subExprValueLoc, err := subExprLoc.Dereference()
			if err != nil {
				return nil, nil, err
			}

			return bc, subExprValueLoc, nil
		default:
			return nil, nil, fmt.Errorf("invalid lhs %T", expr)
		}
	default:
		return nil, nil, fmt.Errorf("invalid lhs %T", expr)
	}
}

func (c *Compiler) compileBCLHSDotExpression(ctx context.Context, prog *Program, expr *DotExpression, typ Type, scope *ValueScope, receiverLoc *Location) (BytecodeSnippet, *Location, error) {
	switch typ := resolveType(typ).(type) {
	case *TupleType:
		index, err := strconv.Atoi(expr.Key)
		if err != nil {
			return nil, nil, expr.WrapError(fmt.Errorf("cannot index tuple with %q", expr.Key))
		}

		if index >= len(typ.Elems()) {
			return nil, nil, expr.WrapError(fmt.Errorf("tuple index %d out bounds on tuple with %d elements", index, len(typ.Elems())))
		}

		elemLoc, err := receiverLoc.IndexTuple(index)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return nil, elemLoc, nil
	case *StructType:
		fieldLoc, err := receiverLoc.IndexFieldConst(expr.Key)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return nil, fieldLoc, nil
	case *PointerType:
		receiverLocValue, err := receiverLoc.Dereference()
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return c.compileBCLHSDotExpression(ctx, prog, expr, typ.Pointee(), scope, receiverLocValue)
	default:
		return nil, nil, expr.WrapError(fmt.Errorf("cannot dot index receiver type %s", dereferenceType(expr.Receiver.Type())))
	}
}

func (c *Compiler) compileBCLHSIndexExpression(ctx context.Context, prog *Program, expr *IndexExpression, typ Type, scope *ValueScope, receiverLoc, indexLoc *Location) (BytecodeSnippet, *Location, error) {
	switch resolveType(typ).(type) {
	case *ArrayType:
		elemLoc, err := receiverLoc.IndexArray(indexLoc)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return nil, elemLoc, nil
	case *SliceType:
		elemLoc, err := receiverLoc.IndexSlice(indexLoc)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return nil, elemLoc, nil
	case *MapType:
		// TODO:
		return nil, nil, expr.WrapError(fmt.Errorf("map type not yet implemented"))
	default:
		return nil, nil, expr.WrapError(fmt.Errorf("cannot index receiver type %s", expr.Receiver.Type()))
	}
}

func (c *Compiler) compileBCStatement(ctx context.Context, prog *Program, stmt Statement, scope *ValueScope) (BytecodeSnippet, error) {
	var bc BytecodeSnippet
	switch stmt := stmt.(type) {
	case *VarStatement:
		var err error
		var initBC []Bytecode
		var rhsLoc *Location

		local := scope.newLocal(stmt.Variable.Name(), stmt.Type)

		if stmt.Expression != nil {
			initBC, rhsLoc, err = c.compileBCExpression(ctx, prog, stmt.Expression, scope, local)
			if err != nil {
				return nil, err
			}
		} else {
			initBC, rhsLoc, err = c.compileBCZeroValue(ctx, prog, stmt.Position, stmt.Type, scope, local)
			if err != nil {
				return nil, err
			}
		}

		bc.Add(initBC...)

		if rhsLoc != local {
			bc.Add(scope.Mov(rhsLoc, local))
		}

		return bc, nil
	case *DeclarationStatement:
		local := scope.newLocal(stmt.Variable.Name(), stmt.Expression.Type())

		initBC, rhsOp, err := c.compileBCExpression(ctx, prog, stmt.Expression, scope, local)
		if err != nil {
			return nil, err
		}

		bc.Add(initBC...)

		if rhsOp != local {
			bc.Add(scope.Mov(rhsOp, local))
		}

		return bc, nil
	case *AssignmentStatement:
		lhsBC, lhsOp, err := c.compileBCLHS(ctx, prog, stmt.Left, scope)
		if err != nil {
			return nil, err
		}

		rhsBC, rhsOp, err := c.compileBCExpression(ctx, prog, stmt.Right, scope, lhsOp)
		if err != nil {
			return nil, err
		}

		bc.Add(lhsBC...)
		bc.Add(rhsBC...)
		if lhsOp != rhsOp {
			bc.Add(scope.Mov(rhsOp, lhsOp))
		}

		return bc, nil
	case *AssignmentOperatorStatement:
		lhsBC, lhsLoc, err := c.compileBCLHS(ctx, prog, stmt.Left, scope)
		if err != nil {
			return nil, err
		}

		rhsLoc := scope.allocTemp(stmt.Right.Type())
		defer scope.deallocTemp(rhsLoc)

		rhsBC, rhsLoc, err := c.compileBCExpression(ctx, prog, stmt.Right, scope, rhsLoc)
		if err != nil {
			return nil, err
		}

		bc.Add(lhsBC...)
		bc.Add(rhsBC...)

		op, err := stmt.Operator.AssignmentToInfix()
		if err != nil {
			return nil, stmt.WrapError(err)
		}

		bc.BinOp(lhsLoc, lhsLoc, op, rhsLoc)

		return bc, nil
	case *PostfixStatement:
		lhsBC, lhsLoc, err := c.compileBCLHS(ctx, prog, stmt.Expression, scope)
		if err != nil {
			return nil, err
		}

		bc.Add(lhsBC...)

		op, err := stmt.Operator.PostfixToInfix()
		if err != nil {
			return nil, stmt.WrapError(err)
		}

		bc.BinOp(lhsLoc, lhsLoc, op, scope.newImmediate(Int(1)))

		return bc, nil
	case *ReturnStatement:
		var offset Size
		offset += stmt.Function.Return().Size()
		offset += stmt.Function.Receiver().Type().Size()
		for _, param := range stmt.Function.Parameters() {
			offset += param.Type().Size()
		}

		if stmt.Expression != nil {
			var exprBC []Bytecode
			var err error

			returnVar, _ := scope.Get("__return")
			exprBC, exprLoc, err := c.compileBCExpression(ctx, prog, stmt.Expression, scope, returnVar)
			if err != nil {
				return nil, err
			}

			bc.Add(exprBC...)

			if exprLoc != returnVar {
				bc.Mov(returnVar, exprLoc)
			}
		}

		bc.Add(Ret{
			Args: offset,
		})

		return bc, nil
	case *ForStatement:
		forScope := scope.sub(stmt.Scope)

		var err error
		var initBC []Bytecode
		if stmt.Init != nil {
			initBC, err = c.compileBCStatement(ctx, prog, stmt.Init, forScope)
			if err != nil {
				return nil, err
			}
		}

		var loopBC BytecodeSnippet

		var bodyBC []Bytecode
		for _, subStmt := range stmt.Body {
			bodyStmtBC, err := c.compileBCStatement(ctx, prog, subStmt, forScope)
			if err != nil {
				return nil, err
			}

			bodyBC = append(bodyBC, bodyStmtBC...)
		}

		var stepBC []Bytecode
		if stmt.Step != nil {
			stepBC, err = c.compileBCStatement(ctx, prog, stmt.Step, forScope)
			if err != nil {
				return nil, err
			}
		}

		end := newLabel()

		var condBC BytecodeSnippet
		if stmt.Condition != nil {
			condTmp := forScope.allocTemp(stmt.Condition.Type())
			defer forScope.deallocTemp(condTmp)

			var condLoc *Location

			condBC, condLoc, err = c.compileBCExpression(ctx, prog, stmt.Condition, forScope, condTmp)
			if err != nil {
				return nil, err
			}

			condBC.JumpAfter(end, condLoc.Operand.Not())
		}

		bc.Add(initBC...)
		loop := newLabel()
		loopBC.Add(condBC...)
		loopBC.Add(bodyBC...)
		loopBC.Add(stepBC...)
		loopBC.JumpTo(loop, nil)
		loopBC.LabelFirst(loop)
		bc.Add(loopBC...)
		bc.LabelLast(end)

		return bc, nil
	case *ExpressionStatement:
		if stmt.Expression.Type() != nil {
			tmp := scope.allocTemp(stmt.Expression.Type())
			defer scope.deallocTemp(tmp)

			exprBC, _, err := c.compileBCExpression(ctx, prog, stmt.Expression, scope, tmp)
			if err != nil {
				return nil, err
			}

			bc.Add(exprBC...)
		} else {
			exprBC, _, err := c.compileBCExpression(ctx, prog, stmt.Expression, scope, nil)
			if err != nil {
				return nil, err
			}

			bc.Add(exprBC...)
		}

		return bc, nil
	case *IfStatement:
		var condBC BytecodeSnippet
		var condLoc *Location

		if stmt.Condition != nil {
			condReg := scope.allocTemp(stmt.Condition.Type())

			var err error
			condBC, condLoc, err = c.compileBCExpression(ctx, prog, stmt.Condition, scope, condReg)
			if err != nil {
				return nil, err
			}

			scope.deallocTemp(condReg)
		}

		bodyScope := scope.sub(stmt.Scope)

		var bodyBC []Bytecode
		for _, subStmt := range stmt.Body {
			bodyStmtBC, err := c.compileBCStatement(ctx, prog, subStmt, bodyScope)
			if err != nil {
				return nil, err
			}

			bodyBC = append(bodyBC, bodyStmtBC...)
		}

		elseLabel := newLabel()
		endLabel := newLabel()

		if stmt.Condition != nil {
			if stmt.Else == nil {
				elseLabel = endLabel
			}
			condBC.JumpAfter(elseLabel, condLoc.Operand.Not())
		}

		bc.Add(condBC...)
		bc.Add(bodyBC...)
		if stmt.Condition != nil {
		}

		if stmt.Else != nil {
			bc.JumpAfter(endLabel, nil)
			bc.LabelLast(elseLabel)
			elseBC, err := c.compileBCStatement(ctx, prog, stmt.Else, scope)
			if err != nil {
				return nil, err
			}

			bc.Add(elseBC...)
		}

		bc.LabelLast(endLabel)

		return bc, nil
	default:
		return nil, stmt.WrapError(fmt.Errorf("unhandled statement %T in bytecode generator", stmt))
	}
}

func (c *Compiler) compileBCZeroValue(ctx context.Context, prog *Program, pos parser.Position, typ Type, scope *ValueScope, dst *Location) (BytecodeSnippet, *Location, error) {
	var bc BytecodeSnippet

	switch typ := resolveType(typ).(type) {
	case TypeKind:
		var imm Immediate
		switch typ.Kind() {
		case KindInt:
			imm = Int(0)
		case KindFloat:
			imm = Float(0)
		case KindString:
			return bc, scope.getString(""), nil
		case KindBool:
			imm = Bool(false)
		default:
			return nil, nil, pos.WrapError(fmt.Errorf("unhandled zero value for type %s", typ))
		}
		return nil, scope.newImmediate(imm), nil
	case *PointerType:
		return nil, scope.newImmediate(Int(0)), nil
	case *StructType:
		var offset Size
		for _, field := range typ.Fields() {
			fieldDst, err := dst.IndexFieldConst(field.Name)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}

			fieldBC, fieldLoc, err := c.compileBCZeroValue(ctx, prog, pos, field.Type, scope, fieldDst)
			if err != nil {
				return nil, nil, err
			}

			offset += field.Type.Size()

			bc.Add(fieldBC...)

			if fieldLoc != fieldDst {
				bc.Mov(fieldDst, fieldLoc)
			}
		}

		return bc, dst, nil
	case *ArrayType:
		elem := typ.Elem()

		var offset Size
		for i := range typ.Length() {
			elemDst, err := dst.IndexArrayConst(i)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}

			elemBC, elemLoc, err := c.compileBCZeroValue(ctx, prog, pos, elem, scope, elemDst)
			if err != nil {
				return nil, nil, err
			}

			offset += elem.Size()

			bc.Add(elemBC...)

			if elemLoc != elemDst {
				bc.Mov(elemDst, elemLoc)
			}
		}

		return bc, dst, nil
	case *TupleType:
		var offset Size
		for i, elem := range typ.Elems() {
			elemDst, err := dst.IndexTuple(i)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}

			elemBC, elemLoc, err := c.compileBCZeroValue(ctx, prog, pos, elem, scope, dst)
			if err != nil {
				return nil, nil, err
			}

			offset += elem.Size()

			bc.Add(elemBC...)

			if elemLoc != elemDst {
				bc.Mov(elemDst, elemLoc)
			}
		}

		return bc, dst, nil
	case *InterfaceType:
		valBC, err := c.compileBCValuesLiteral(ctx, prog, []Expression{
			NewLiteral(NilType.GlobalName()),
			NewLiteral(NilValue),
		}, scope, dst.AsType(interfaceTuple))
		if err != nil {
			return nil, nil, err
		}

		bc.Add(valBC...)

		return bc, dst, nil
	case *SliceType:
		valBC, valDst, err := c.compileBCZeroValue(ctx, prog, pos, sliceHeader, scope, dst.AsType(sliceHeader))
		if err != nil {
			return nil, nil, err
		}

		bc.Add(valBC...)

		if !valDst.AsType(typ).SameMemory(dst) {
			bc.Mov(dst, valDst)
		}

		return bc, dst, nil
	case *FunctionType:
		return bc, scope.newImmediate(Int(0)).AsType(typ), nil
	default:
		return nil, nil, pos.WrapError(fmt.Errorf("unhandled zero value for type %s", typ))
	}
}

func (c *Compiler) compileBCExpression(ctx context.Context, prog *Program, expr Expression, scope *ValueScope, dst *Location) ([]Bytecode, *Location, error) {
	var bc BytecodeSnippet
	switch expr := expr.(type) {
	case *Literal:
		switch expr.Type().Kind() {
		case KindNil:
			return c.compileBCZeroValue(ctx, prog, expr.Position, dst.Type, scope, dst)
		default:
			return nil, expr.Value().Location(scope), nil
		}
	case *CompilerFunctionReferenceExpression:
		return nil, scope.newFunctionRef(expr.Function), nil
	case *SymbolReferenceExpression:
		sym, ok := scope.Get(expr.Name())
		if !ok {
			return nil, nil, expr.WrapError(fmt.Errorf("unknown symbol %q", expr.Name()))
		}

		return nil, sym, nil
	case *ParenthesizedExpression:
		return c.compileBCExpression(ctx, prog, expr.Expression, scope, dst)
	case *BuiltinSymbol:
		return nil, nil, expr.WrapError(fmt.Errorf("builtin %q used as value", expr.Name()))
	case *BinaryExpression:
		var lhsTmp *Location
		if expr.Left.Type().Size() == dst.Type.Size() {
			lhsTmp = dst.AsType(expr.Left.Type())
		} else {
			lhsTmp := scope.allocTemp(expr.Left.Type())
			defer scope.deallocTemp(lhsTmp)
		}

		lhsBC, lhsLoc, err := c.compileBCExpression(ctx, prog, expr.Left, scope, lhsTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(lhsBC...)

		rhsTmp := scope.allocTemp(expr.Right.Type())
		rhsBC, rhsLoc, err := c.compileBCExpression(ctx, prog, expr.Right, scope, rhsTmp)
		if err != nil {
			return nil, nil, err
		}
		defer scope.deallocTemp(rhsTmp)

		bc.Add(rhsBC...)

		if expr.Operator.CanOperand() && expr.Type().Kind().IsPrimitive() && expr.Left.Type().Kind().IsPrimitive() && expr.Right.Type().Kind().IsPrimitive() {
			return bc, &Location{
				Kind: LocationKindLocal,
				Type: dst.Type,
				Operand: &Operand{
					Kind: OperandKindBinary,
					Value: BinaryOperand{
						Left:  lhsLoc.Operand,
						Op:    expr.Operator,
						Right: rhsLoc.Operand,
					},
				},
			}, nil
		}

		bc.BinOp(dst, lhsLoc, expr.Operator, rhsLoc)

		return bc, dst, nil
	case *UnaryExpression:
		switch expr.Operator {
		case OperatorAddress:
			tmp := scope.allocTemp(expr.Expression.Type())
			defer scope.deallocTemp(tmp)

			srcBC, srcLoc, err := c.compileBCExpression(ctx, prog, expr.Expression, scope, tmp)
			if err != nil {
				return nil, nil, err
			}

			bc.Add(srcBC...)

			return bc, srcLoc.AddressOf(), nil
		case OperatorDereference:
			tmp := scope.allocTemp(expr.Expression.Type())
			defer scope.deallocTemp(tmp)

			srcBC, srcOp, err := c.compileBCExpression(ctx, prog, expr.Expression, scope, tmp)
			if err != nil {
				return nil, nil, err
			}

			bc.Add(srcBC...)

			srcLocValue, err := srcOp.Dereference()
			if err != nil {
				return nil, nil, expr.WrapError(err)
			}

			return bc, srcLocValue, nil
		default:
			srcBC, srcOp, err := c.compileBCExpression(ctx, prog, expr.Expression, scope, dst)
			if err != nil {
				return nil, nil, err
			}

			bc.Add(srcBC...)

			bc.Add(UnOp{
				Op:  UnaryOperation(expr.Operator, expr.Expression.Type().Kind()),
				Src: srcOp.Operand,
				Dst: dst.Operand,
			})

			return bc, dst, nil
		}
	case *CallExpression:
		callScope := scope.sub(scope.symbols)

		switch ftype := resolveType(expr.Function.Type()).(type) {
		case *FunctionType:
			callTmp := callScope.allocTemp(expr.Function.Type())
			defer callScope.deallocTemp(callTmp)

			retVar := callScope.newArg("__return", 0, ftype.Return)

			recvVar := callScope.newArg("__recv", 0, ftype.Receiver)

			if ftype.Return.Size() > 0 {
				bc.Mov(scope.SP(), scope.SP().AddConst(ftype.Return.Size()))
			}

			callBC, callLoc, err := c.compileBCCallReceiverExpression(ctx, prog, expr.Function, callScope, recvVar, callTmp)
			if err != nil {
				return nil, nil, err
			}

			bc.Add(callBC...)

			variadicParam, hasVariadic := ftype.Parameters[len(ftype.Parameters)-1].(*VariadicType)
			for i, arg := range expr.Args {
				if i >= len(ftype.Parameters)-1 && hasVariadic {
					break
				}

				argVar := callScope.newArg(fmt.Sprintf("%d", i), 0, ftype.Parameters[i])
				argTmp := callScope.allocTemp(arg.Type())

				argBC, argLoc, err := c.compileBCExpression(ctx, prog, arg, callScope, argTmp)
				if err != nil {
					return nil, nil, err
				}

				callScope.deallocTemp(argTmp)

				bc.Add(argBC...)

				bc.Mov(argVar, argLoc)

				bc.Mov(scope.SP(), scope.SP().AddConst(arg.Type().Size()))
			}

			// TODO: handle spread operator

			// TODO: fix mixed concrete and variadic parameters
			if hasVariadic {
				varTmp := callScope.allocTemp(variadicParam)
				defer callScope.deallocTemp(varTmp)

				varArg := callScope.newArg("...", 0, variadicParam)

				varBC, varLoc, err := c.compileBCZeroValue(ctx, prog, expr.Position, sliceHeader, scope, varTmp.AsType(sliceHeader))
				if err != nil {
					return nil, nil, err
				}

				bc.Add(varBC...)

				for _, arg := range expr.Args[len(ftype.Parameters)-1:] {
					argTmp := callScope.allocTemp(arg.Type())
					argBC, argLoc, err := c.compileBCExpression(ctx, prog, arg, callScope, argTmp)
					if err != nil {
						return nil, nil, err
					}

					callScope.deallocTemp(argTmp)

					bc.Add(argBC...)

					bc.App(varLoc, varLoc, argLoc, variadicParam.Elem().Size())
				}

				bc.Mov(varArg, varLoc)
				bc.Mov(scope.SP(), scope.SP().AddConst(variadicParam.Size()))
			}

			bc.Add(Cal{
				Func: callLoc.Operand,
			})

			if retVar.Type.Size() > 0 && !retVar.SameMemory(dst) {
				bc.Mov(dst, retVar)
			}

			return bc, dst, nil
		case *TypeType:
			if expr.Args[0].Type().Kind() == KindUnknown {
				return nil, nil, expr.WrapError(fmt.Errorf("cannot convert unknown type to %s", ftype.Type))
			}

			switch ftype.Type.Kind() {
			case expr.Args[0].Type().Kind():
				argsBC, argsLoc, err := c.compileBCExpression(ctx, prog, expr.Args[0], scope, dst)
				if err != nil {
					return nil, nil, err
				}

				bc.Add(argsBC...)

				// basic type conversions are no-ops
				return bc, argsLoc, nil
			default:
				return nil, nil, expr.WrapError(fmt.Errorf("type conversions not yet implemented"))
			}
		default:
			return nil, nil, expr.WrapError(fmt.Errorf("cannot call non-function type %v", ftype))
		}
	case *DotExpression:
		receiverTmp := scope.allocTemp(expr.Receiver.Type())
		defer scope.deallocTemp(receiverTmp)
		receiverBC, receiverLoc, err := c.compileBCExpression(ctx, prog, expr.Receiver, scope, receiverTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(receiverBC...)

		return c.compileBCDotExpression(ctx, prog, expr, expr.Receiver.Type(), scope, receiverLoc)
	case *IndexExpression:
		receiverTmp := scope.allocTemp(expr.Receiver.Type())
		defer scope.deallocTemp(receiverTmp)
		receiverBC, receiverLoc, err := c.compileBCExpression(ctx, prog, expr.Receiver, scope, receiverTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(receiverBC...)

		indexTmp := scope.allocTemp(expr.Index.Type())
		defer scope.deallocTemp(indexTmp)
		indexBC, indexLoc, err := c.compileBCExpression(ctx, prog, expr.Index, scope, indexTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(indexBC...)

		return c.compileBCIndexExpression(ctx, prog, expr, expr.Receiver.Type(), scope, receiverLoc, indexLoc)
	case *TupleExpression:
		for i, elem := range expr.Elems {
			dst, err := dst.IndexTuple(i)
			if err != nil {
				return nil, nil, expr.WrapError(err)
			}

			elemBC, elemLoc, err := c.compileBCExpression(ctx, prog, elem, scope, dst)
			if err != nil {
				return nil, nil, expr.WrapError(err)
			}

			bc.Add(elemBC...)

			if dst != elemLoc {
				bc.Mov(dst, elemLoc)
			}
		}

		return bc, dst, nil
	case *ArrayExpression:
		for i, elem := range expr.Elems {
			dst, err := dst.IndexArrayConst(i)
			if err != nil {
				return nil, nil, err
			}

			elemBC, elemLoc, err := c.compileBCExpression(ctx, prog, elem, scope, dst)
			if err != nil {
				return nil, nil, err
			}

			bc.Add(elemBC...)

			if dst != elemLoc {
				bc.Mov(dst, elemLoc)
			}
		}

		return bc, dst, nil
	case *InterfaceTypeCoercionExpression:
		ifaceDst := dst.AsType(interfaceTuple)
		typElem, err := ifaceDst.IndexTuple(0)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		bc.Mov(typElem, scope.typeName(expr.Expression.Type()))

		valElem, err := ifaceDst.IndexTuple(1)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		valElem = valElem.AsType(expr.Expression.Type())

		if expr.Expression.Type().Size() > 1 {
			return nil, nil, expr.WrapError(fmt.Errorf("currently interfaces with values of size > 1 not implemented"))
		}

		valBC, valLoc, err := c.compileBCExpression(ctx, prog, expr.Expression, scope, valElem)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(valBC...)

		if valLoc != valElem {
			bc.Mov(valElem, valLoc)
		}

		return bc, dst, nil
	case *TypeExpression:
		return bc, scope.typeName(expr.typ), nil
	case *BuiltinExpression:
		return expr.Impl.Compile(ctx, c, prog, expr.Position, expr.Args, scope, dst)
	default:
		return nil, nil, expr.WrapError(fmt.Errorf("unhandled expression in bytecode generator %T", expr))
	}
}

func (c *Compiler) compileBCCallReceiverExpression(ctx context.Context, prog *Program, expr Expression, scope *ValueScope, recvDst, callDst *Location) (BytecodeSnippet, *Location, error) {
	var bc BytecodeSnippet
	switch expr := expr.(type) {
	case *MethodExpression:
		recvBC, recvLoc, err := c.compileBCExpression(ctx, prog, expr.Receiver, scope, recvDst)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(recvBC...)

		methodName := expr.Method.Name

		if expr.Receiver.Type().Kind() == KindInterface {
			iface := resolveType(expr.Receiver.Type()).(*InterfaceType)

			method, ok := iface.Methods().Get(methodName)
			if !ok {
				return nil, nil, expr.WrapError(fmt.Errorf("could not resolve method %s on type %s", methodName, expr.Receiver.Type()))
			}

			typLoc, err := recvLoc.AsType(interfaceTuple).IndexTuple(0)
			if err != nil {
				return nil, nil, expr.WrapError(err)
			}

			valLoc, err := recvLoc.AsType(interfaceTuple).IndexTuple(1)
			if err != nil {
				return nil, nil, expr.WrapError(err)
			}

			// TODO: handle wide receivers
			bc.Mov(recvDst, valLoc)
			bc.Mov(scope.SP(), scope.SP().AddConst(1))

			return bc, scope.vtableLookup(typLoc, method), nil
		} else {
			if recvLoc != recvDst {
				bc.Mov(recvDst, recvLoc)
			}

			if expr.Receiver.Type().Size() > 0 {
				bc.Mov(scope.SP(), scope.SP().AddConst(expr.Receiver.Type().Size()))
			}

			methodFunc, ok := TypeMethod(expr.Receiver.Type(), methodName)
			if !ok {
				return nil, nil, expr.WrapError(fmt.Errorf("could not resolve method %s on type %s", methodName, expr.Receiver.Type()))
			}

			callLoc, ok := scope.Get(methodFunc.Name())
			if !ok {
				return nil, nil, expr.WrapError(fmt.Errorf("could not resolve location for method %q", methodFunc.Name()))
			}

			return bc, callLoc, nil
		}
	case *ParenthesizedExpression:
		return c.compileBCCallReceiverExpression(ctx, prog, expr.Expression, scope, recvDst, callDst)
	default:
		return c.compileBCExpression(ctx, prog, expr, scope, callDst)
	}
}

func (c *Compiler) compileBCDotExpression(ctx context.Context, prog *Program, expr *DotExpression, typ Type, scope *ValueScope, receiverLoc *Location) (BytecodeSnippet, *Location, error) {
	switch typ := resolveType(typ).(type) {
	case *TupleType:
		index, err := strconv.Atoi(expr.Key)
		if err != nil {
			return nil, nil, expr.WrapError(fmt.Errorf("cannot index tuple with %q", expr.Key))
		}

		elemLoc, err := receiverLoc.IndexTuple(index)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return nil, elemLoc, nil
	case *PointerType:
		receiverLocValue, err := receiverLoc.Dereference()
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return c.compileBCDotExpression(ctx, prog, expr, typ.Pointee(), scope, receiverLocValue)
	case *StructType:
		fieldLoc, err := receiverLoc.IndexFieldConst(expr.Key)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return nil, fieldLoc, nil
	case *TypeType:
		method, ok := TypeMethod(typ.Type, expr.Key)
		if !ok {
			return nil, nil, expr.WrapError(fmt.Errorf("could not resolve method %s on type %s", expr.Key, typ))
		}

		loc, ok := scope.Get(method.Name())
		if !ok {
			return nil, nil, expr.WrapError(fmt.Errorf("could not resolve method %s on type %s", expr.Key, typ))
		}

		return nil, loc, nil
	default:
		return nil, nil, expr.WrapError(fmt.Errorf("cannot dot index receiver type %s", dereferenceType(expr.Receiver.Type())))
	}
}

func (c *Compiler) compileBCIndexExpression(ctx context.Context, prog *Program, expr *IndexExpression, typ Type, scope *ValueScope, receiverLoc, indexLoc *Location) (BytecodeSnippet, *Location, error) {
	switch resolveType(typ).(type) {
	case *ArrayType:
		elemLoc, err := receiverLoc.IndexArray(indexLoc)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return nil, elemLoc, nil
	case *SliceType:
		elemLoc, err := receiverLoc.IndexSlice(indexLoc)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return nil, elemLoc, nil
	case *MapType:
		// TODO:
		return nil, nil, expr.WrapError(fmt.Errorf("map type not yet implemented"))
	default:
		return nil, nil, expr.WrapError(fmt.Errorf("cannot index receiver type %s", expr.Receiver.Type()))
	}
}

func (c *Compiler) compileBCValuesLiteral(ctx context.Context, prog *Program, exprs []Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, error) {
	var bc BytecodeSnippet
	var offset Size

	for i, expr := range exprs {
		elemDst, err := dst.IndexTuple(i)
		if err != nil {
			return nil, err
		}

		exprBC, exprOp, err := c.compileBCExpression(ctx, prog, expr, scope, elemDst)
		if err != nil {
			return nil, err
		}

		bc.Add(exprBC...)
		if exprOp != elemDst {
			bc.Mov(elemDst, exprOp)
		}

		offset += expr.Type().Size()
	}

	return bc, nil
}
