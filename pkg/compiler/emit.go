package compiler

import (
	"context"
	"fmt"
	"log"
	"slices"
	"strconv"

	"github.com/rhino1998/aeon/pkg/compiler/air"
	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/operators"
	"github.com/rhino1998/aeon/pkg/compiler/types"
	"github.com/rhino1998/aeon/pkg/parser"
	"github.com/rhino1998/aeon/pkg/topological"
)

func (c *Compiler) compileBC(ctx context.Context, prog *Program) error {
	// TODO: sort by import orderings
	registerOperands := make([]*air.Operand, prog.Registers())

	for reg := range air.Register(prog.Registers()) {
		if reg < 3 {
			continue
		}
		registerOperands[reg] = air.IntOperand(0)
		prog.bytecode.Add(air.Mov{
			Src:  registerOperands[reg],
			Dst:  air.RegisterOperand(reg),
			Size: 1,
		})
	}

	prog.values = BuiltinValues(prog.Registers(), prog.root)
	prog.registerType(types.Void)
	prog.registerType(TypeInt)
	prog.registerType(TypeFloat)
	prog.registerType(TypeBool)
	prog.registerType(TypeString)
	prog.registerType(TypeNil)
	prog.registerType(air.FuncTuple)
	prog.registerType(air.ExternFuncTuple)
	prog.registerType(air.SliceTuple)
	prog.registerType(air.InterfaceTuple)

	// skipNull := NewVariable("#aaa_nil_page", types.NewArray(16, TypeInt), prog.root)
	// prog.root.put(skipNull)
	// heapAllocs := NewVariable(heapAllocSliceName, air.SliceTuple, prog.root)
	// prog.root.put(heapAllocs)
	// heapStrAllocs := NewVariable(heapStrAllocSliceName, air.SliceTuple, prog.root)
	// prog.root.put(heapStrAllocs)
	//
	// heapGlobals := []*Variable{skipNull, heapAllocs, heapStrAllocs}
	//
	// for _, global := range heapGlobals {
	// 	globBC, err := c.compileGlobal(ctx, prog, prog.values, global)
	// 	if err != nil {
	// 		return err
	// 	}
	//
	// 	prog.bytecode.Add(globBC...)
	// }

	for _, pkg := range prog.Packages() {
		err := c.compileBCPackage(ctx, pkg)
		if err != nil {
			return err
		}

		prog.bytecode.Mount(pkg)

		for _, typ := range pkg.KnownTypes() {
			name := typ.GlobalName()
			if other, ok := prog.types[name]; ok {
				if !types.Equal(typ, other) {
					panic(fmt.Errorf("non-equal duplicate types %s %s", typ, other))
				}
			}
		}

		for _, str := range pkg.Strings() {
			prog.strings[str] = struct{}{}
		}
	}

	var err error
	prog.globalSize, prog.globalLayout, err = prog.bytecode.ResolveGlobals(0)
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

	for i, bc := range prog.bytecode {
		log.Printf("%02x: %v", i, bc)
	}

	prog.bytecode.OptimizeOperands()

	return nil
}

func (c *Compiler) compileBCPackage(ctx context.Context, pkg *Package) error {
	pkg.values = pkg.prog.values.sub(pkg.scope)

	for _, constant := range pkg.Constants() {
		val, err := constant.Evaluate()
		if err != nil {
			return err
		}

		pkg.values.newConstant(constant.Name(), constant.Type(), val.Value().Operand)
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

	err = pkg.scope.put(varInitFunc)
	if err != nil {
		return err
	}

	return nil
}

func (c *Compiler) compileGlobal(ctx context.Context, prog *Program, scope *ValueScope, global *Variable) (air.Snippet, error) {
	var bc air.Snippet
	ptr := scope.newGlobal(global)
	if global.expr != nil {
		exprBC, exprLoc, err := c.compileBCExpression(ctx, prog, global.expr, scope, ptr)
		if err != nil {
			return nil, err
		}

		bc.Add(exprBC...)

		err = bc.Mov(ptr, exprLoc)
		if err != nil {
			return nil, global.WrapError(err)
		}
	} else {
		exprBC, exprLoc, err := c.compileBCZeroValue(ctx, prog, global.Position, global.Type(), scope, ptr)
		if err != nil {
			return nil, err
		}

		bc.Add(exprBC...)

		err = bc.Mov(ptr, exprLoc)
		if err != nil {
			return nil, global.WrapError(err)
		}
	}

	return bc, nil
}

func (c *Compiler) compilePackageVarInit(ctx context.Context, pkg *Package) (*Function, error) {
	f := newFunction(VarInitFuncName, pkg)
	f.ret = types.Void
	f.receiver = &Variable{typ: types.Void}

	scope := pkg.values
	// TODO: special varinit tmp var semantics

	numLocals := scope.newImmediate(air.Int(0))

	f.bytecode.Mov(scope.SP(), scope.SP().Add(numLocals))
	f.bytecode.Label(air.Label(f.QualifiedName()))

	// TODO: make these truly global
	for _, externFunc := range pkg.prog.root.ExternFunctions() {
		fName := externFunc.Name()
		hdr := scope.newGlobal(NewVariable("@"+fName, air.ExternFuncTuple, scope.symbols))
		hdrBC, err := c.compileBCValuesLiteral(ctx, pkg.prog, []Expression{
			NewLiteral(air.Int(1)),
			NewLiteral(air.String(externFunc.Name())),
			NewLiteral(air.String("extern")),
			NewLiteral(air.String(externFunc.Name())),
		}, scope, hdr)
		if err != nil {
			return nil, err
		}

		f.bytecode.Add(hdrBC...)

		scope.newConstant(externFunc.Name(), externFunc.Type(), hdr.Operand.AddressOf())
	}

	for _, externFunc := range pkg.ExternFunctions() {
		fName := externFunc.Name()
		hdr := scope.newGlobal(NewVariable("@"+fName, air.ExternFuncTuple, scope.symbols))
		hdrBC, err := c.compileBCValuesLiteral(ctx, pkg.prog, []Expression{
			NewLiteral(air.Int(1)),
			NewLiteral(air.String(externFunc.Name())),
			NewLiteral(air.String("extern")),
			NewLiteral(air.String(externFunc.Name())),
		}, scope, hdr)
		if err != nil {
			return nil, err
		}

		f.bytecode.Add(hdrBC...)

		scope.newConstant(externFunc.Name(), externFunc.Type(), hdr.Operand.AddressOf())
	}

	{
		fName := f.Name()
		hdr := scope.newGlobal(NewVariable("@"+fName, air.FuncTuple, scope.symbols))
		hdrBC, err := c.compileBCValuesLiteral(ctx, pkg.prog, []Expression{
			NewLiteral(air.Int(2)),
			NewLiteral(air.String(f.QualifiedName())),
			NewLiteral(air.String(f.Position.File)),
			&CompilerFunctionReferenceExpression{f},
		}, scope, hdr)
		if err != nil {
			return nil, err
		}

		f.bytecode.Add(hdrBC...)

		f.SetInfoAddr(hdr.Operand.AddressOf())

		scope.newConstant(fName, f.Type(), hdr.Operand.AddressOf())
		scope.newConstant(f.QualifiedName(), f.Type(), hdr.Operand.AddressOf())
	}

	for _, fun := range pkg.Functions() {
		fName := fun.Name()
		hdr := scope.newGlobal(NewVariable("@"+fName, air.FuncTuple, scope.symbols))
		hdrBC, err := c.compileBCValuesLiteral(ctx, pkg.prog, []Expression{
			NewLiteral(air.Int(2)),
			NewLiteral(air.String(fun.QualifiedName())),
			NewLiteral(air.String(fun.Position.File)),
			&CompilerFunctionReferenceExpression{fun},
		}, scope, hdr)
		if err != nil {
			return nil, err
		}

		f.bytecode.Add(hdrBC...)

		fun.SetInfoAddr(hdr.Operand.AddressOf())

		scope.newConstant(fName, fun.Type(), hdr.Operand.AddressOf())
		scope.newConstant(fun.QualifiedName(), f.Type(), hdr.Operand.AddressOf())
	}

	// check for circular dependencies
	for _, symbol := range pkg.Globals() {
		global := symbol.Dereference().(*Variable)
		deps := global.SymbolDependencies(nil)

		if slices.ContainsFunc(deps, func(sym *SymbolReference) bool {
			return sym.QualifiedName() == global.QualifiedName()
		}) {
			return nil, global.WrapError(fmt.Errorf("circular dependency in global %s", global.Name()))
		}
	}

	globalSymbolsSorted, err := topological.SortFunc(
		mapSlice(pkg.Globals(), (*Variable).Reference),
		(*SymbolReference).QualifiedName,
		func(s *SymbolReference) []*SymbolReference {
			return s.Dereference().(*Variable).SymbolDependencies(nil)
		},
	)
	if err != nil {
		return nil, err
	}

	for _, symbol := range globalSymbolsSorted {
		global := symbol.Dereference().(*Variable)
		globBC, err := c.compileGlobal(ctx, pkg.prog, scope, global)
		if err != nil {
			return nil, err
		}

		f.bytecode.Add(globBC...)
	}

	f.bytecode.Ret(0)

	// for i, bc := range f.bytecode {
	// 	log.Printf("%02x: %v", i, bc)
	// }

	localSize, locals, err := f.bytecode.ResolveLocals()
	if err != nil {
		return f, err
	}

	f.locals = locals
	numLocals.Operand.Value = air.Int(localSize)

	pkg.varinit = f

	return f, nil
}

func (c *Compiler) compileBCFunction(ctx context.Context, f *Function, scope *ValueScope) error {
	scope = scope.fun(f.symbols)
	scope.getString(air.String(f.QualifiedName()))

	numLocals := scope.newImmediate(air.Int(0))
	f.bytecode.Mov(scope.SP(), scope.SP().Add(numLocals))
	f.bytecode.Label(air.Label(f.QualifiedName()))

	funcStackSize, err := air.FunctionStackSize(f.Type().(*types.Function))
	if err != nil {
		return f.WrapError(err)
	}

	baseOffset := -(f.pkg.prog.FrameSize() + funcStackSize)

	var offset air.Size = 1
	scope.newParam("__return", baseOffset+offset, f.Return())
	retSize, err := air.TypeSize(f.Return())
	if err != nil {
		return f.WrapError(err)
	}
	offset += retSize

	if f.Receiver().Name() != "" {
		scope.newParam(f.Receiver().Name(), baseOffset+offset, f.Receiver().Type())
	}
	recvSize, err := air.TypeSize(f.Receiver().Type())
	if err != nil {
		return f.WrapError(err)
	}
	offset += recvSize

	for _, param := range f.Parameters() {
		if param.Name() != "" {
			scope.newParam(param.Name(), baseOffset+offset, param.Type())
		}

		paramSize, err := air.TypeSize(param.Type())
		if err != nil {
			return f.WrapError(err)
		}

		offset += paramSize
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
	if _, ok := last.(*ReturnStatement); !ok && f.Return() == types.Void {
		f.bytecode.Ret(0)
	}

	// for i, bc := range f.bytecode {
	// 	log.Printf("%02x: %v", i, bc)
	// }

	// Can resolve here for debugging
	// err := f.bytecode.ResolveLabels()
	// if err != nil {
	// 	return err
	// }
	//
	localSize, locals, err := f.bytecode.ResolveLocals()
	if err != nil {
		return err
	}

	f.locals = locals

	numLocals.Operand.Value = air.Int(localSize)
	var _ = locals

	return nil
}

func (c *Compiler) compileBCGlobal(ctx context.Context, prog *Program, name string, expr Expression, scope *ValueScope, dst *air.Value) (air.Snippet, error) {
	var bc air.Snippet
	globBC, globLoc, err := c.compileBCExpression(ctx, prog, expr, scope, dst)
	if err != nil {
		return nil, err
	}

	bc.Add(globBC...)

	bc.Mov(dst, globLoc)

	return bc, nil
}

func (c *Compiler) compileBCLHS(ctx context.Context, prog *Program, expr Expression, scope *ValueScope) (air.Snippet, *air.Value, error) {
	var bc air.Snippet
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
		receiverBC, receiverLoc, err := c.compileBCExpression(ctx, prog, expr.Receiver, scope, receiverTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(receiverBC...)

		indexTmp := scope.allocTemp(expr.Index.Type())
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
		case operators.Dereference:
			tmp := scope.allocTemp(expr.Expression.Type())
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
			return nil, nil, expr.WrapError(fmt.Errorf("invalid lhs %T", expr))
		}
	case *DiscardExpression:
		return nil, scope.allocTemp(expr.Type()), nil
	default:
		return nil, nil, expr.WrapError(fmt.Errorf("invalid lhs %T", expr))
	}
}

func (c *Compiler) compileBCLHSDotExpression(ctx context.Context, prog *Program, expr *DotExpression, typ types.Type, scope *ValueScope, receiverLoc *air.Value) (air.Snippet, *air.Value, error) {
	switch typ := types.Resolve(typ).(type) {
	case *types.Tuple:
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
	case *types.Struct:
		fieldLoc, err := receiverLoc.IndexField(expr.Key)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return nil, fieldLoc, nil
	case *types.Pointer:
		receiverLocValue, err := receiverLoc.Dereference()
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return c.compileBCLHSDotExpression(ctx, prog, expr, typ.Pointee(), scope, receiverLocValue)
	default:
		return nil, nil, expr.WrapError(fmt.Errorf("cannot dot index receiver type %s", types.Dereference(expr.Receiver.Type())))
	}
}

func (c *Compiler) compileBCLHSIndexExpression(ctx context.Context, prog *Program, expr *IndexExpression, typ types.Type, scope *ValueScope, receiverLoc, indexLoc *air.Value) (air.Snippet, *air.Value, error) {
	switch types.Resolve(typ).(type) {
	case *types.Array:
		elemLoc, err := receiverLoc.IndexArray(indexLoc)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return nil, elemLoc, nil
	case *types.Slice:
		elemLoc, err := receiverLoc.IndexSlice(indexLoc)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return nil, elemLoc, nil
	case *types.Map:
		// TODO:
		return nil, nil, expr.WrapError(fmt.Errorf("map type not yet implemented"))
	default:
		return nil, nil, expr.WrapError(fmt.Errorf("cannot index receiver type %s", expr.Receiver.Type()))
	}
}

func (c *Compiler) compileBCStatement(ctx context.Context, prog *Program, stmt Statement, scope *ValueScope) (air.Snippet, error) {
	var bc air.Snippet
	switch stmt := stmt.(type) {
	case *VarStatement:
		var err error
		var initBC []air.Instruction
		var rhsVal *air.Value

		local := scope.newLocal(stmt.Variable)

		if stmt.Expression != nil {
			initBC, rhsVal, err = c.compileBCExpression(ctx, prog, stmt.Expression, scope, local)
			if err != nil {
				return nil, err
			}
		} else {
			initBC, rhsVal, err = c.compileBCZeroValue(ctx, prog, stmt.Position, stmt.Type, scope, local)
			if err != nil {
				return nil, err
			}
		}

		bc.Add(initBC...)

		err = bc.Mov(local, rhsVal)
		if err != nil {
			return nil, stmt.WrapError(err)
		}

		return bc, nil
	case *DeclarationStatement:
		if len(stmt.Variables) == 1 {
			local := scope.newLocal(stmt.Variables[0])

			initBC, rhsVal, err := c.compileBCExpression(ctx, prog, stmt.Expression, scope, local)
			if err != nil {
				return nil, err
			}

			bc.Add(initBC...)

			err = bc.Mov(local, rhsVal)
			if err != nil {
				return nil, stmt.WrapError(err)
			}

			return bc, nil
		} else {
			locals := make([]*air.Value, len(stmt.Variables))
			for i := range stmt.Variables {
				locals[i] = scope.newLocal(stmt.Variables[i])
			}

			tupleVal := locals[0].AsType(stmt.Expression.Type())

			initBC, rhsVal, err := c.compileBCExpression(ctx, prog, stmt.Expression, scope, tupleVal)
			if err != nil {
				return nil, err
			}

			bc.Add(initBC...)

			err = bc.Mov(tupleVal, rhsVal)
			if err != nil {
				return nil, stmt.WrapError(err)
			}

			return bc, nil
		}
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
		err = bc.Mov(lhsOp, rhsOp)
		if err != nil {
			return nil, stmt.WrapError(err)
		}

		return bc, nil
	case *AssignmentOperatorStatement:
		lhsBC, lhsLoc, err := c.compileBCLHS(ctx, prog, stmt.Left, scope)
		if err != nil {
			return nil, err
		}

		rhsLoc := scope.allocTemp(stmt.Right.Type())

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

		opBC, opLoc, err := c.compileBCBinaryElementwiseOperator(ctx, prog, scope, stmt.Position, op, lhsLoc, lhsLoc, rhsLoc)
		if err != nil {
			return nil, err
		}

		bc.Add(opBC...)
		bc.Mov(lhsLoc, opLoc)

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

		bc.BinOp(lhsLoc, lhsLoc, op, stmt.Expression.Type().Kind(), scope.newImmediate(air.Int(1)))

		return bc, nil
	case *ReturnStatement:
		if stmt.Expression != nil {
			var exprBC []air.Instruction
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

		funcStackSize, err := air.FunctionStackSize(stmt.Function.Type().(*types.Function))
		if err != nil {
			return nil, stmt.WrapError(err)
		}

		bc.Ret(funcStackSize)

		return bc, nil
	case *ForStatement:
		forScope := scope.sub(stmt.Scope)

		var err error
		var initBC []air.Instruction
		if stmt.Init != nil {
			initBC, err = c.compileBCStatement(ctx, prog, stmt.Init, forScope)
			if err != nil {
				return nil, err
			}
		}

		var loopBC air.Snippet

		var bodyBC []air.Instruction
		for _, subStmt := range stmt.Body {
			bodyStmtBC, err := c.compileBCStatement(ctx, prog, subStmt, forScope)
			if err != nil {
				return nil, err
			}

			bodyBC = append(bodyBC, bodyStmtBC...)
		}

		var stepBC []air.Instruction
		if stmt.Step != nil {
			stepBC, err = c.compileBCStatement(ctx, prog, stmt.Step, forScope)
			if err != nil {
				return nil, err
			}
		}

		end := air.NewLabel()

		var condBC air.Snippet
		if stmt.Condition != nil {
			condTmp := forScope.allocTemp(stmt.Condition.Type())

			var condLoc *air.Value

			condBC, condLoc, err = c.compileBCExpression(ctx, prog, stmt.Condition, forScope, condTmp)
			if err != nil {
				return nil, err
			}

			condBC.JumpTo(end, condLoc.Not())
		}

		bc.Add(initBC...)
		loop := air.NewLabel()
		loopBC.Nop()
		loopBC.Label(loop)
		loopBC.Add(condBC...)
		loopBC.Add(bodyBC...)
		loopBC.Add(stepBC...)
		loopBC.JumpTo(loop, scope.newImmediate(air.Bool(true)))
		bc.Add(loopBC...)
		bc.Nop()
		bc.Label(end)

		return bc, nil
	case *ExpressionStatement:
		if stmt.Expression.Type() != nil {
			tmp := scope.allocTemp(stmt.Expression.Type())

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
		var condBC air.Snippet
		var condLoc *air.Value

		if stmt.Condition != nil {
			condReg := scope.allocTemp(stmt.Condition.Type())

			var err error
			condBC, condLoc, err = c.compileBCExpression(ctx, prog, stmt.Condition, scope, condReg)
			if err != nil {
				return nil, err
			}
		}

		bodyScope := scope.sub(stmt.Scope)

		var bodyBC []air.Instruction
		for _, subStmt := range stmt.Body {
			bodyStmtBC, err := c.compileBCStatement(ctx, prog, subStmt, bodyScope)
			if err != nil {
				return nil, err
			}

			bodyBC = append(bodyBC, bodyStmtBC...)
		}

		elseLabel := air.NewLabel()
		endLabel := air.NewLabel()

		if stmt.Condition != nil {
			if stmt.Else == nil {
				elseLabel = endLabel
			}
			condBC.JumpTo(elseLabel, condLoc.Not())
		}

		bc.Add(condBC...)
		bc.Add(bodyBC...)

		if stmt.Else != nil {
			bc.JumpTo(endLabel, scope.newImmediate(air.Bool(true)))
			bc.Nop()
			bc.Label(elseLabel)
			elseBC, err := c.compileBCStatement(ctx, prog, stmt.Else, scope)
			if err != nil {
				return nil, err
			}

			bc.Add(elseBC...)
		}

		bc.Nop()
		bc.Label(endLabel)

		return bc, nil
	default:
		return nil, stmt.WrapError(fmt.Errorf("unhandled statement %T in bytecode generator", stmt))
	}
}

func (c *Compiler) compileBCZeroValue(ctx context.Context, prog *Program, pos parser.Position, typ types.Type, scope *ValueScope, dst *air.Value) (air.Snippet, *air.Value, error) {
	var bc air.Snippet

	switch typ := types.Resolve(typ).(type) {
	case types.Kind:
		var imm air.Immediate
		switch typ.Kind() {
		case kinds.Int:
			imm = air.Int(0)
		case kinds.Float:
			imm = air.Float(0)
		case kinds.String:
			return bc, scope.getString(""), nil
		case kinds.Bool:
			imm = air.Bool(false)
		default:
			return nil, nil, pos.WrapError(fmt.Errorf("unhandled zero value for type %s", typ))
		}
		return nil, scope.newImmediate(imm), nil
	case *types.Pointer:
		return nil, &air.Value{
			Type:    typ,
			Operand: air.IntOperand(0),
		}, nil
	case *types.Struct:
		var offset air.Size
		for _, field := range typ.Fields() {
			fieldDst, err := dst.IndexField(field.Name)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}

			fieldBC, fieldLoc, err := c.compileBCZeroValue(ctx, prog, pos, field.Type, scope, fieldDst)
			if err != nil {
				return nil, nil, err
			}

			fieldSize, err := air.TypeSize(field.Type)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}

			offset += fieldSize

			bc.Add(fieldBC...)

			if fieldLoc != fieldDst {
				bc.Mov(fieldDst, fieldLoc)
			}
		}

		return bc, dst, nil
	case *types.Array:
		elem := typ.Elem()

		length := typ.Length()

		var offset air.Size
		for i := range length {
			elemDst, err := dst.IndexArrayConst(i)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}

			elemBC, elemLoc, err := c.compileBCZeroValue(ctx, prog, pos, elem, scope, elemDst)
			if err != nil {
				return nil, nil, err
			}

			elemSize, err := air.TypeSize(elem)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}

			offset += elemSize

			bc.Add(elemBC...)

			if elemLoc != elemDst {
				bc.Mov(elemDst, elemLoc)
			}
		}

		return bc, dst, nil
	case *types.Tuple:
		var offset air.Size
		for i, elem := range typ.Elems() {
			elemDst, err := dst.IndexTuple(i)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}

			elemBC, elemLoc, err := c.compileBCZeroValue(ctx, prog, pos, elem, scope, dst)
			if err != nil {
				return nil, nil, err
			}

			elemSize, err := air.TypeSize(elem)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}

			offset += elemSize

			bc.Add(elemBC...)

			if elemLoc != elemDst {
				bc.Mov(elemDst, elemLoc)
			}
		}

		return bc, dst, nil
	case *types.Interface:
		valBC, err := c.compileBCValuesLiteral(ctx, prog, []Expression{
			NewLiteral(air.TypeName(TypeNil.GlobalName())),
			NewLiteral(air.Int(0)),
		}, scope, dst.AsType(air.InterfaceTuple))
		if err != nil {
			return nil, nil, err
		}

		bc.Add(valBC...)

		return bc, dst, nil
	case *types.Slice:
		valBC, valDst, err := c.compileBCZeroValue(ctx, prog, pos, air.SliceTuple, scope, dst.AsType(air.SliceTuple))
		if err != nil {
			return nil, nil, err
		}

		bc.Add(valBC...)

		err = bc.Mov(dst, valDst)
		if err != nil {
			return nil, nil, pos.WrapError(err)
		}

		return bc, dst, nil
	case *types.Function:
		return bc, scope.newImmediate(air.Int(0)).AsType(typ), nil
	default:
		return nil, nil, pos.WrapError(fmt.Errorf("unhandled zero value for type %s", typ))
	}
}

func (c *Compiler) compileBCExpression(ctx context.Context, prog *Program, expr Expression, scope *ValueScope, dst *air.Value) ([]air.Instruction, *air.Value, error) {
	var bc air.Snippet
	switch expr := expr.(type) {
	case *NilExpression:
		return c.compileBCZeroValue(ctx, prog, expr.Position, expr.Type(), scope, dst)
	case *Literal:
		return nil, expr.Value(scope), nil
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
		// TODO: short-circuiting

		leftTmp := scope.allocTemp(expr.Left.Type())

		leftBC, leftLoc, err := c.compileBCExpression(ctx, prog, expr.Left, scope, leftTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(leftBC...)

		rightTmp := scope.allocTemp(expr.Right.Type())
		rightBC, rightLoc, err := c.compileBCExpression(ctx, prog, expr.Right, scope, rightTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(rightBC...)

		if expr.Operator.IsComparison() {
			binOpBC, binOpLoc, err := c.compileBCBinaryComparison(ctx, prog, scope, expr.Position, expr.Operator, dst, leftLoc, rightLoc)
			if err != nil {
				return nil, nil, err
			}

			bc.Add(binOpBC...)

			return bc, binOpLoc, nil
		} else {
			binOpBC, binOpLoc, err := c.compileBCBinaryElementwiseOperator(ctx, prog, scope, expr.Position, expr.Operator, dst, leftLoc, rightLoc)
			if err != nil {
				return nil, nil, err
			}

			bc.Add(binOpBC...)

			return bc, binOpLoc, nil
		}
	case *UnaryExpression:
		switch expr.Operator {
		case operators.Address:
			tmp := scope.allocTemp(expr.Expression.Type())

			srcBC, srcLoc, err := c.compileBCExpression(ctx, prog, expr.Expression, scope, tmp)
			if err != nil {
				return nil, nil, err
			}

			bc.Add(srcBC...)

			return bc, srcLoc.AddressOf(), nil
		case operators.Dereference:
			tmp := scope.allocTemp(expr.Expression.Type())

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

			bc.Add(air.UnOp{
				Op:   expr.Operator,
				Kind: expr.Expression.Type().Kind(),
				Src:  srcOp.Operand,
				Dst:  dst.Operand,
			})

			return bc, dst, nil
		}
	case *CallExpression:
		callScope := scope.sub(scope.symbols)

		switch ftype := types.Resolve(expr.Function.Type()).(type) {
		case *types.Function:
			callTmp := scope.allocTemp(expr.Function.Type())

			recvTmp := scope.allocTemp(ftype.Receiver)

			callBC, callLoc, recvLoc, err := c.compileBCCallReceiverExpression(ctx, prog, expr.Function, callScope, recvTmp, callTmp)
			if err != nil {
				return nil, nil, err
			}

			bc.Add(callBC...)

			argLocs := make([]*air.Value, 0, len(ftype.Parameters))
			argTmps := make([]*air.Value, 0, len(ftype.Parameters))

			if recvLoc != nil {
				argLocs = append(argLocs, recvLoc)
			}

			var hasVariadic bool
			var variadicParam *types.Variadic
			if len(ftype.Parameters) > 0 {
				variadicParam, hasVariadic = ftype.Parameters[len(ftype.Parameters)-1].(*types.Variadic)
			}

			for i, arg := range expr.Args {
				if i >= len(ftype.Parameters)-1 && hasVariadic {
					break
				}

				argTmps = append(argTmps, scope.allocTemp(arg.Type()))

				argBC, argLoc, err := c.compileBCExpression(ctx, prog, arg, callScope, argTmps[i])
				if err != nil {
					return nil, nil, err
				}

				argLocs = append(argLocs, argLoc)

				bc.Add(argBC...)
			}

			if hasVariadic {
				varTmp := callScope.allocTemp(variadicParam)
				argTmps = append(argTmps, varTmp)

				if len(expr.Args) > len(ftype.Parameters)-1 && expr.Args[len(ftype.Parameters)-1].Type().Kind() == kinds.Variadic {
					varBC, varLoc, err := c.compileBCExpression(ctx, prog, expr.Args[len(ftype.Parameters)-1], callScope, varTmp)
					if err != nil {
						return nil, nil, err
					}

					bc.Add(varBC...)

					argLocs = append(argLocs, varLoc)
				} else {
					// TODO: maybe stack allocate this when escape analysis works
					varBC, varLoc, err := c.compileBCZeroValue(ctx, prog, expr.Position, air.SliceTuple, scope, varTmp.AsType(air.SliceTuple))
					if err != nil {
						return nil, nil, err
					}

					bc.Add(varBC...)

					argLocs = append(argLocs, varLoc)

					for _, arg := range expr.Args[len(ftype.Parameters)-1:] {
						argTmp := scope.allocTemp(arg.Type())
						argBC, argLoc, err := c.compileBCExpression(ctx, prog, arg, callScope, argTmp)
						if err != nil {
							return nil, nil, err
						}

						bc.Add(argBC...)

						err = bc.App(varLoc, varLoc, argLoc)
						if err != nil {
							return nil, nil, expr.WrapError(err)
						}
					}
				}
			}

			retVar := callScope.newArg("__return", ftype.Return)
			retSize, err := air.TypeSize(ftype.Return)
			if err != nil {
				return nil, nil, expr.WrapError(err)
			}

			bc.Mov(scope.SP(), scope.SP().AddConst(retSize))
			for _, argLoc := range argLocs {
				argSize, err := air.TypeSize(argLoc.Type)
				if err != nil {
					return nil, nil, expr.WrapError(err)
				}

				argVar := callScope.newArg("arg", argLoc.Type)
				bc.Mov(argVar, argLoc)
				bc.Mov(scope.SP(), scope.SP().AddConst(argSize))
			}

			bc.Add(air.Cal{
				Func: callLoc.Operand,
				Line: int(expr.Position.Line),
			})

			bc.Mov(dst, retVar)

			return bc, dst, nil
		case *types.TypeType:
			if expr.Args[0].Type().Kind() == kinds.Unknown {
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
		receiverBC, receiverLoc, err := c.compileBCExpression(ctx, prog, expr.Receiver, scope, receiverTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(receiverBC...)

		return c.compileBCDotExpression(ctx, prog, expr, expr.Receiver.Type(), scope, receiverLoc)
	case *IndexExpression:
		receiverTmp := scope.allocTemp(expr.Receiver.Type())
		receiverBC, receiverLoc, err := c.compileBCExpression(ctx, prog, expr.Receiver, scope, receiverTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(receiverBC...)

		indexTmp := scope.allocTemp(expr.Index.Type())
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
	case *TypeLiteralExpression:
		for i, elem := range expr.Elems {
			elemDst, err := dst.IndexArrayConst(i)
			if err != nil {
				return nil, nil, err
			}

			elemBC, elemLoc, err := c.compileBCExpression(ctx, prog, elem, scope, elemDst)
			if err != nil {
				return nil, nil, err
			}

			bc.Add(elemBC...)

			if elemDst != elemLoc {
				bc.Mov(elemDst, elemLoc)
			}
		}

		return bc, dst, nil
	case *InterfaceCoercionExpression:
		ifaceDst := dst.AsType(air.InterfaceTuple)
		typElem, err := ifaceDst.IndexTuple(0)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		typeLoc, err := scope.typeName(expr.Expression.Type())
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		bc.Mov(typElem, typeLoc)

		valElem, err := ifaceDst.IndexTuple(1)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		valElem = valElem.AsType(expr.Expression.Type())

		exprSize, err := air.TypeSize(expr.Expression.Type())
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		if exprSize > 1 {
			valBoxPtr := valElem.AsType(types.NewPointer(expr.Expression.Type()))
			bc.AllocConst(valBoxPtr, exprSize)

			valBox, err := valBoxPtr.Dereference()
			if err != nil {
				return nil, nil, expr.WrapError(err)
			}

			valBC, valLoc, err := c.compileBCExpression(ctx, prog, expr.Expression, scope, valBox)
			if err != nil {
				return nil, nil, err
			}

			bc.Add(valBC...)
			bc.Mov(valBox, valLoc)
		} else {
			valBC, valLoc, err := c.compileBCExpression(ctx, prog, expr.Expression, scope, valElem)
			if err != nil {
				return nil, nil, err
			}

			bc.Add(valBC...)
			bc.Mov(valElem, valLoc)
		}

		return bc, dst, nil
	case *TypeExpression:
		typeLoc, err := scope.typeName(expr.typ)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}
		return bc, typeLoc, nil
	case *BuiltinExpression:
		return expr.Impl.Compile(ctx, c, prog, expr.Position, expr.Args, scope, dst)
	case *SpreadExpression:
		sliceBC, sliceLoc, err := c.compileBCExpression(ctx, prog, expr.Expr, scope, dst.AsType(expr.Expr.Type()))
		if err != nil {
			return nil, nil, err
		}

		bc.Add(sliceBC...)
		return bc, sliceLoc.AsType(expr.Type()), nil
	case *ErrorReturnExpression:
		tmp := scope.allocTemp(expr.Expr.Type())

		subExprBC, subExprLoc, err := c.compileBCExpression(ctx, prog, expr.Expr, scope, tmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(subExprBC...)

		ret, ok := scope.Get("__return")
		if !ok {
			return nil, nil, expr.WrapError(fmt.Errorf("could not find return value"))
		}
		var errRetLoc *air.Value
		switch expr.Function.Return().Kind() {
		case kinds.Interface:
			errRetLoc = ret.AsType(air.InterfaceTuple)
		case kinds.Tuple:
			tupleTyp := types.Resolve(scope.function.Return()).(*types.Tuple)
			errRetLoc, err = ret.IndexTuple(len(tupleTyp.Elems()) - 1)
			if err != nil {
				return nil, nil, expr.WrapError(err)
			}

			errRetLoc = errRetLoc.AsType(air.InterfaceTuple)
		default:
			return nil, nil, expr.WrapError(fmt.Errorf("cannot return error from function with return type %s", expr.Function.Return()))
		}

		var errLoc *air.Value
		switch expr.Expr.Type().Kind() {
		case kinds.Interface:
			errLoc = subExprLoc.AsType(air.InterfaceTuple)
		case kinds.Tuple:
			errLoc, err = subExprLoc.IndexTuple(len(expr.Expr.Type().(*types.Tuple).Elems()) - 1)
			if err != nil {
				return nil, nil, expr.WrapError(err)
			}
			errLoc = errLoc.AsType(air.InterfaceTuple)
		default:
			return nil, nil, expr.WrapError(fmt.Errorf("cannot return error from expression of type %s", expr.Expr.Type()))
		}

		nilTypeVal, err := scope.typeName(TypeNil)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		errorSize, err := air.TypeSize(TypeError)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		skipRet := air.NewLabel()
		bc.JumpTo(skipRet, must1(errLoc.InterfaceType()).Equal(nilTypeVal))

		errHandlerLoc, ok := scope.Get(errorHandlerSymbolName)
		if !ok {
			bc.Mov(errRetLoc, errLoc)
		} else {
			handlerScope := scope.sub(scope.symbols)
			handlerRet := handlerScope.newArg("__return", expr.Function.Return())
			handlerArg := handlerScope.newArg("err", TypeError)
			bc.Mov(scope.SP(), scope.SP().AddConst(errorSize))
			bc.Mov(handlerArg, errLoc)
			bc.Mov(scope.SP(), scope.SP().AddConst(errorSize))
			bc.Add(air.Cal{
				Func: errHandlerLoc.Operand,
				Line: int(expr.Position.Line),
			})
			bc.JumpTo(skipRet, must1(handlerRet.InterfaceType()).Equal(nilTypeVal))
			bc.Mov(errRetLoc, handlerRet)
		}

		funcStackSize, err := air.FunctionStackSize(scope.function.Type().(*types.Function))
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		bc.Ret(funcStackSize)

		bc.Mov(dst, tmp.AsType(dst.Type))
		bc.Label(skipRet)

		return bc, dst, nil
	case *ErrorHandlerExpression:
		handlerDefScope := scope.sub(scope.symbols)

		// TODO: error handle scope
		errHandlerVar := NewVariable(errorHandlerSymbolName, errorHandlerFunctionType, scope.symbols)

		handlerScope := handlerDefScope.sub(scope.symbols)

		handlerDst := scope.newLocal(errHandlerVar)

		handlerBC, handlerLoc, err := c.compileBCExpression(ctx, prog, expr.Handler, handlerDefScope, handlerDst)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(handlerBC...)
		if handlerLoc != handlerDst {
			bc.Mov(handlerDst, handlerLoc)
		}

		subExprBC, subExprLoc, err := c.compileBCExpression(ctx, prog, expr.Expr, handlerScope, dst)
		if err != nil {
			return nil, nil, err
		}
		bc.Add(subExprBC...)

		return bc, subExprLoc, nil
	case *UnknownExpression:
		// NOTE: this assumes that other errors have already occurred
		return nil, scope.allocTemp(types.Unknown), nil
	default:
		return nil, nil, expr.WrapError(fmt.Errorf("unhandled expression in bytecode generator %T", expr))
	}
}

func (c *Compiler) compileBCCallReceiverExpression(ctx context.Context, prog *Program, expr Expression, scope *ValueScope, recvDst, callDst *air.Value) (air.Snippet, *air.Value, *air.Value, error) {
	var bc air.Snippet
	switch expr := expr.(type) {
	case *MethodExpression:
		recvBC, recvLoc, err := c.compileBCExpression(ctx, prog, expr.Receiver, scope, recvDst)
		if err != nil {
			return nil, nil, nil, err
		}

		bc.Add(recvBC...)

		methodName := expr.Method.Name

		if expr.Receiver.Type().Kind() == kinds.Interface {
			iface := types.Resolve(expr.Receiver.Type()).(*types.Interface)

			method, ok := iface.Methods().Get(methodName)
			if !ok {
				return nil, nil, nil, expr.WrapError(fmt.Errorf("could not resolve method %s on type %s", methodName, expr.Receiver.Type()))
			}

			typLoc, err := recvLoc.InterfaceType()
			if err != nil {
				return nil, nil, nil, expr.WrapError(err)
			}

			valLoc, err := recvLoc.InterfaceValue()
			if err != nil {
				return nil, nil, nil, expr.WrapError(err)
			}

			return bc, scope.vtableLookup(typLoc, method), valLoc, nil
		} else {
			methodFunc, ok := types.Methods(expr.Receiver.Type()).Get(methodName)
			if !ok {
				return nil, nil, nil, expr.WrapError(fmt.Errorf("could not resolve method %s on type %s", methodName, expr.Receiver.Type()))
			}

			callLoc, ok := scope.Get(methodFunc.QualifiedName())
			if !ok {
				return nil, nil, nil, expr.WrapError(fmt.Errorf("could not resolve location for method %q", methodFunc.Name))
			}

			return bc, callLoc, recvLoc, nil
		}
	case *ParenthesizedExpression:
		return c.compileBCCallReceiverExpression(ctx, prog, expr.Expression, scope, recvDst, callDst)
	default:
		bc, callLoc, err := c.compileBCExpression(ctx, prog, expr, scope, callDst)
		return bc, callLoc, nil, err
	}
}

func (c *Compiler) compileBCDotExpression(ctx context.Context, prog *Program, expr *DotExpression, typ types.Type, scope *ValueScope, receiverLoc *air.Value) (air.Snippet, *air.Value, error) {
	switch typ := types.Resolve(typ).(type) {
	case *types.Tuple:
		index, err := strconv.Atoi(expr.Key)
		if err != nil {
			return nil, nil, expr.WrapError(fmt.Errorf("cannot index tuple with %q", expr.Key))
		}

		elemLoc, err := receiverLoc.IndexTuple(index)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return nil, elemLoc, nil
	case *types.Pointer:
		receiverLocValue, err := receiverLoc.Dereference()
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return c.compileBCDotExpression(ctx, prog, expr, typ.Pointee(), scope, receiverLocValue)
	case *types.Struct:
		fieldLoc, err := receiverLoc.IndexField(expr.Key)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return nil, fieldLoc, nil
	case *types.TypeType:
		method, ok := types.Methods(typ.Type).Get(expr.Key)
		if !ok {
			return nil, nil, expr.WrapError(fmt.Errorf("could not resolve method %s on type %s", expr.Key, typ))
		}

		loc, ok := scope.Get(method.Name)
		if !ok {
			return nil, nil, expr.WrapError(fmt.Errorf("could not resolve method %s on type %s", expr.Key, typ))
		}

		return nil, loc, nil
	default:
		return nil, nil, expr.WrapError(fmt.Errorf("cannot dot index receiver type %s", types.Dereference(expr.Receiver.Type())))
	}
}

func (c *Compiler) compileBCIndexExpression(ctx context.Context, prog *Program, expr *IndexExpression, typ types.Type, scope *ValueScope, receiverLoc, indexLoc *air.Value) (air.Snippet, *air.Value, error) {
	switch types.Resolve(typ).(type) {
	case *types.Array:
		elemLoc, err := receiverLoc.IndexArray(indexLoc)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return nil, elemLoc, nil
	case *types.Slice:
		elemLoc, err := receiverLoc.IndexSlice(indexLoc)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return nil, elemLoc, nil
	case *types.Map:
		// TODO:
		return nil, nil, expr.WrapError(fmt.Errorf("map type not yet implemented"))
	default:
		return nil, nil, expr.WrapError(fmt.Errorf("cannot index receiver type %s", expr.Receiver.Type()))
	}
}

func (c *Compiler) compileBCValuesLiteral(ctx context.Context, prog *Program, exprs []Expression, scope *ValueScope, dst *air.Value) (air.Snippet, error) {
	var bc air.Snippet
	var offset air.Size

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

		exprSize, err := air.TypeSize(expr.Type())
		if err != nil {
			return nil, err
		}

		offset += exprSize
	}

	return bc, nil
}

func (c *Compiler) compileBCBinaryElementwiseOperator(ctx context.Context, prog *Program, scope *ValueScope, pos parser.Position, op operators.Operator, dst, left, right *air.Value) (air.Snippet, *air.Value, error) {
	var bc air.Snippet
	if !types.Equal(types.Base(left.Type), types.Base(right.Type)) {
		return nil, nil, pos.WrapError(fmt.Errorf("invalid binary operator %s on types %s and %s", op, left.Type, right.Type))
	}

	if left.Type.Kind().IsPrimitive() && op.CanOperand() {
		return bc, &air.Value{
			Type: dst.Type,
			Operand: &air.Operand{
				Kind: air.OperandKindBinary,
				Value: air.BinaryOperand{
					Left:  left.Operand,
					Op:    op,
					Right: right.Operand,
				},
			},
		}, nil
	}

	switch left.Type.Kind() {
	case kinds.Int, kinds.String, kinds.Bool, kinds.Pointer, kinds.Float, kinds.Type, kinds.Function:
		bc.BinOp(dst, left, op, left.Type.Kind(), right)
		return bc, dst, nil
	case kinds.Slice:
		if op.IsComparison() {
			return c.compileBCBinaryElementwiseOperator(ctx, prog, scope, pos, op, dst, left.AsType(air.SliceTuple), right.AsType(air.SliceTuple))
		} else {
			return c.compileBCBinaryElementwiseOperator(ctx, prog, scope, pos, op, dst.AsType(air.SliceTuple), left.AsType(air.SliceTuple), right.AsType(air.SliceTuple))
		}
	case kinds.Interface:
		// TODO: handle wide interfaces
		if op.IsComparison() {
			return c.compileBCBinaryElementwiseOperator(ctx, prog, scope, pos, op, dst, left.AsType(air.InterfaceTuple), right.AsType(air.InterfaceTuple))
		} else {
			return c.compileBCBinaryElementwiseOperator(ctx, prog, scope, pos, op, dst.AsType(air.InterfaceTuple), left.AsType(air.InterfaceTuple), right.AsType(air.InterfaceTuple))
		}
	case kinds.Tuple:
		opDst := dst

		for i := range types.Resolve(left.Type).(*types.Tuple).Elems() {
			elemDst, err := opDst.IndexTuple(i)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			elemLeft, err := left.IndexTuple(i)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			elemRight, err := right.IndexTuple(i)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			elemBC, elemLoc, err := c.compileBCBinaryElementwiseOperator(ctx, prog, scope, pos, op, elemDst, elemLeft, elemRight)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			bc.Add(elemBC...)
			if elemLoc != elemDst {
				bc.Mov(elemDst, elemLoc)
			}
		}

		return bc, dst, nil
	case kinds.Array:
		length := types.Resolve(left.Type).(*types.Array).Length()

		for i := range length {
			elemDst, err := dst.IndexArrayConst(i)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			elemLeft, err := left.IndexArrayConst(i)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			elemRight, err := right.IndexArrayConst(i)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			elemBC, elemLoc, err := c.compileBCBinaryElementwiseOperator(ctx, prog, scope, pos, op, elemDst, elemLeft, elemRight)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			bc.Add(elemBC...)
			if elemLoc != elemDst {
				bc.Mov(elemDst, elemLoc)
			}
		}

		return bc, dst, nil
	case kinds.Struct:
		for _, field := range types.Resolve(left.Type).(*types.Struct).Fields() {
			elemDst, err := dst.IndexField(field.Name)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			elemLeft, err := left.IndexField(field.Name)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			elemRight, err := right.IndexField(field.Name)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			elemBC, elemLoc, err := c.compileBCBinaryElementwiseOperator(ctx, prog, scope, pos, op, elemDst, elemLeft, elemRight)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			bc.Add(elemBC...)
			if elemLoc != elemDst {
				bc.Mov(elemDst, elemLoc)
			}
		}

		return bc, dst, nil
	case kinds.Map:
		// TODO:
		return nil, nil, pos.WrapError(fmt.Errorf("map type not yet implemented"))
	default:
		return nil, nil, pos.WrapError(fmt.Errorf("cannot binary operator %s on type %s", op, left.Type))
	}
}

func (c *Compiler) compileBCBinaryComparison(ctx context.Context, prog *Program, scope *ValueScope, pos parser.Position, op operators.Operator, dst, left, right *air.Value) (air.Snippet, *air.Value, error) {
	var bc air.Snippet
	if !types.Equal(types.Base(left.Type), types.Base(right.Type)) {
		return nil, nil, pos.WrapError(fmt.Errorf("invalid binary operator %s on types %s and %s", op, left.Type, right.Type))
	}

	if left.Type.Kind().IsPrimitive() && op.CanOperand() {
		return bc, &air.Value{
			Type: dst.Type,
			Operand: &air.Operand{
				Kind: air.OperandKindBinary,
				Value: air.BinaryOperand{
					Left:  left.Operand,
					Op:    op,
					Right: right.Operand,
				},
			},
		}, nil
	}

	switch left.Type.Kind() {
	case kinds.Int, kinds.String, kinds.Bool, kinds.Pointer, kinds.Float, kinds.Type, kinds.Function:
		bc.BinOp(dst, left, op, left.Type.Kind(), right)
		return bc, dst, nil
	case kinds.Slice:
		if op.IsComparison() {
			return c.compileBCBinaryComparison(ctx, prog, scope, pos, op, dst, left.AsType(air.SliceTuple), right.AsType(air.SliceTuple))
		} else {
			return c.compileBCBinaryComparison(ctx, prog, scope, pos, op, dst.AsType(air.SliceTuple), left.AsType(air.SliceTuple), right.AsType(air.SliceTuple))
		}
	case kinds.Interface:
		// TODO: handle wide interfaces
		if op.IsComparison() {
			return c.compileBCBinaryComparison(ctx, prog, scope, pos, op, dst, left.AsType(air.InterfaceTuple), right.AsType(air.InterfaceTuple))
		} else {
			return c.compileBCBinaryComparison(ctx, prog, scope, pos, op, dst.AsType(air.InterfaceTuple), left.AsType(air.InterfaceTuple), right.AsType(air.InterfaceTuple))
		}
	case kinds.Tuple:
		if op != operators.Equal && op != operators.NotEqual {
			return nil, nil, pos.WrapError(fmt.Errorf("cannot use operator %s on type %s", op, left.Type))
		}

		mergeOp := operators.LogicalOr
		if op == operators.Equal {
			mergeOp = operators.LogicalAnd
		}

		tmp := scope.allocTemp(types.Kind(kinds.Bool))

		bc.Mov(dst, scope.newImmediate(air.Bool(op == operators.Equal)))

		for i := range types.Resolve(left.Type).(*types.Tuple).Elems() {
			elemLeft, err := left.IndexTuple(i)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			elemRight, err := right.IndexTuple(i)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			elemBC, elemLoc, err := c.compileBCBinaryComparison(ctx, prog, scope, pos, op, tmp, elemLeft, elemRight)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			bc.Add(elemBC...)

			bc.BinOp(dst, dst, mergeOp, dst.Type.Kind(), elemLoc)
		}

		return bc, dst, nil
	case kinds.Array:
		if op != operators.Equal && op != operators.NotEqual {
			return nil, nil, pos.WrapError(fmt.Errorf("cannot use operator %s on type %s", op, left.Type))
		}

		mergeOp := operators.LogicalOr
		if op == operators.Equal {
			mergeOp = operators.LogicalAnd
		}

		tmp := scope.allocTemp(types.Kind(kinds.Bool))

		bc.Mov(dst, scope.newImmediate(air.Bool(op == operators.Equal)))

		length := types.Resolve(left.Type).(*types.Array).Length()

		for i := range length {
			elemLeft, err := left.IndexArrayConst(i)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			elemRight, err := right.IndexArrayConst(i)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			elemBC, elemLoc, err := c.compileBCBinaryComparison(ctx, prog, scope, pos, op, tmp, elemLeft, elemRight)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			bc.Add(elemBC...)

			bc.BinOp(dst, dst, mergeOp, dst.Type.Kind(), elemLoc)
		}

		return bc, dst, nil
	case kinds.Struct:
		if op != operators.Equal && op != operators.NotEqual {
			return nil, nil, pos.WrapError(fmt.Errorf("cannot use operator %s on type %s", op, left.Type))
		}

		mergeOp := operators.LogicalOr
		if op == operators.Equal {
			mergeOp = operators.LogicalAnd
		}

		tmp := scope.allocTemp(types.Kind(kinds.Bool))

		bc.Mov(dst, scope.newImmediate(air.Bool(op == operators.Equal)))

		for _, field := range types.Resolve(left.Type).(*types.Struct).Fields() {
			elemLeft, err := left.IndexField(field.Name)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			elemRight, err := right.IndexField(field.Name)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			elemBC, elemLoc, err := c.compileBCBinaryComparison(ctx, prog, scope, pos, op, tmp, elemLeft, elemRight)
			if err != nil {
				return nil, nil, pos.WrapError(err)
			}
			bc.Add(elemBC...)

			// TODO: short-circuit
			bc.BinOp(dst, dst, mergeOp, dst.Type.Kind(), elemLoc)
		}

		return bc, dst, nil
	case kinds.Map:
		// TODO:
		return nil, nil, pos.WrapError(fmt.Errorf("map type not yet implemented"))
	default:
		return nil, nil, pos.WrapError(fmt.Errorf("cannot binary operator %s on type %s", op, left.Type))
	}
}
