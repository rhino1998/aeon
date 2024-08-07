package compiler

import (
	"context"
	"fmt"
	"log"
	"strconv"
)

func (prog *Program) compileBytecode(ctx context.Context) error {
	// TODO: sort by import orderings
	registerOperands := make([]*Operand, 0)

	for reg := range Register(prog.registers) {
		registerOperands[reg] = ImmediateOperand(Int(0))
		prog.bytecode.Add(Mov{
			Src:  registerOperands[reg],
			Dst:  RegisterOperand(reg),
			Size: 1,
		})
	}

	for _, pkg := range prog.Packages() {
		err := pkg.compileBytecode(ctx)
		if err != nil {
			return err
		}

		prog.bytecode.Mount(pkg)
	}

	return nil
}

func (pkg *Package) compileBytecode(ctx context.Context) error {
	scope := NewValueScope(pkg.prog.registers, pkg.scope)

	for _, constant := range pkg.Constants() {
		// TODO: evaluate const exprs
		switch expr := constant.expr.(type) {
		case *Literal[String]:
			scope.newConstant(constant.Name(), ImmediateOperand(expr.Value()))
		case *Literal[Int]:
			scope.newConstant(constant.Name(), ImmediateOperand(expr.Value()))
		case *Literal[Bool]:
			scope.newConstant(constant.Name(), ImmediateOperand(expr.Value()))
		case *Literal[Float]:
			scope.newConstant(constant.Name(), ImmediateOperand(expr.Value()))
		default:
			return expr.WrapError(fmt.Errorf("invalid constant type %T", expr))
		}
	}

	varInitFunc, err := pkg.compileVarInit(ctx, scope)
	if err != nil {
		return err
	}

	pkg.bytecode.Mount(varInitFunc)

	for _, fun := range pkg.Functions() {
		err := fun.compileBytecode(ctx, scope)
		if err != nil {
			return err
		}

		pkg.bytecode.Mount(fun)
	}

	for i, bc := range pkg.bytecode {
		log.Printf("%02x: %v", i, bc)
	}

	err = pkg.scope.put(varInitFunc)
	if err != nil {
		return err
	}

	return nil
}

func (pkg *Package) compileVarInit(ctx context.Context, scope *ValueScope) (*Function, error) {
	f := newFunction("varinit", pkg)

	scope = scope.sub(pkg.scope)

	f.bytecode.Add(scope.Push("__funcname", IntType, ImmediateOperand(
		String(fmt.Sprintf("func %s.%s", f.Package().Name(), f.Name())),
	)))

	numLocals := ImmediateOperand(Int(0))

	f.bytecode.Add(BinOp{
		Op:    BinaryOperation(KindInt, OperatorAddition, KindInt),
		Dst:   OperandRegisterSP,
		Left:  OperandRegisterSP,
		Right: numLocals,
	})

	for _, global := range pkg.Globals() {
		ptr := scope.newGlobal(global.Name(), global.Type())
		if global.expr != nil {
			exprBC, exprOp, err := pkg.prog.compileBCExpression(ctx, global.expr, scope, ptr)
			if err != nil {
				return nil, err
			}

			f.bytecode.Add(exprBC...)

			if exprOp != ptr {
				f.bytecode.Add(Mov{
					Src:  exprOp,
					Dst:  ptr,
					Size: global.Type().Size(),
				})
			}
		} else {
			exprBC, exprOp, err := pkg.prog.compileBCZeroValue(ctx, global.Type(), scope, ptr)
			if err != nil {
				return nil, err
			}

			f.bytecode.Add(exprBC...)

			if exprOp != ptr {
				f.bytecode.Add(Mov{
					Src:  exprOp,
					Dst:  ptr,
					Size: global.Type().Size(),
				})
			}
		}
	}

	for _, externFunc := range pkg.ExternFunctions() {
		ptr := scope.newGlobal(externFunc.Name(), externType)
		hdrBC, err := pkg.prog.compileBCValuesLiteral(ctx, []Expression{
			NewLiteral(Int(0)),
			NewLiteral(String(externFunc.Name())),
			NewLiteral(Int(0)),
			NewLiteral(String(externFunc.Name())),
		}, scope, ptr)
		if err != nil {
			return nil, err
		}

		f.bytecode.Add(hdrBC...)
	}

	for _, fun := range pkg.Functions() {
		ptr := scope.newGlobal(fun.Name(), funcType)
		hdrBC, err := pkg.prog.compileBCValuesLiteral(ctx, []Expression{
			NewLiteral(Int(0)),
			NewLiteral(String(fun.Name())),
			NewLiteral(Int(0)),
			newOperandExpression(fun.AddrOp(), KindInt),
		}, scope, ptr)
		if err != nil {
			return nil, err
		}

		f.bytecode.Add(hdrBC...)
	}

	f.bytecode.Add(Return{})

	numLocals.Value = Int(*scope.maxLocal)

	return f, nil
}

func (f *Function) compileBytecode(ctx context.Context, scope *ValueScope) error {
	scope = scope.sub(f.symbols)

	f.bytecode.Add(scope.Push("__funcname", IntType, ImmediateOperand(
		String(fmt.Sprintf("func %s.%s", f.Package().Name(), f.Name())),
	)))

	numLocals := ImmediateOperand(Int(0))

	f.bytecode.Add(BinOp{
		Op:    BinaryOperation(KindInt, OperatorAddition, KindInt),
		Dst:   OperandRegisterSP,
		Left:  OperandRegisterSP,
		Right: numLocals,
	})

	var offset AddrOffset = 1
	for _, param := range f.Parameters() {
		if param.Name() == "" {
			continue
		}

		scope.newParam(param.Name(), -AddrOffset(f.pkg.prog.FrameSize()+offset), param.Type())
		offset += param.Type().Size()
	}

	if f.Return() != nil {
		scope.newParam("__return", -AddrOffset(f.pkg.prog.FrameSize()+offset), f.Return())
	}

	for _, stmt := range f.Body() {
		bcs, err := f.pkg.prog.compileBCStatement(ctx, stmt, scope)
		if err != nil {
			return err
		}

		f.bytecode.Add(bcs...)
	}

	numLocals.Value = Int(*scope.maxLocal)

	return nil
}

func (prog *Program) compileBCGlobal(ctx context.Context, name string, expr Expression, scope *ValueScope, dst *Operand) (BytecodeSnippet, error) {
	var bc BytecodeSnippet
	globBC, globOp, err := prog.compileBCExpression(ctx, expr, scope, dst)
	if err != nil {
		return nil, err
	}

	bc.Add(globBC...)

	if globOp != dst {
		bc.Add(Mov{
			Src:  globOp,
			Dst:  dst,
			Size: expr.Type().Size(),
		})
	}

	return bc, nil
}

func (prog *Program) compileBCLHS(ctx context.Context, expr Expression, scope *ValueScope) (BytecodeSnippet, *Operand, error) {
	var bc BytecodeSnippet
	switch expr := expr.(type) {
	case *SymbolReferenceExpression:
		op, ok := scope.Get(expr.Name())
		if !ok {
			return nil, nil, expr.WrapError(fmt.Errorf("cannot assign to unknown symbol %q", expr.Name()))
		}
		return nil, op, nil
	case *ParenthesizedExpression:
		return prog.compileBCLHS(ctx, expr, scope)
	case *DotExpression:
		receiverTmp := scope.allocTemp(expr.Receiver.Type())
		defer scope.deallocTemp(receiverTmp)
		receiverBC, receiverOp, err := prog.compileBCExpression(ctx, expr.Receiver, scope, receiverTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(receiverBC...)

		switch typ := BaseType(expr.Receiver.Type()).(type) {
		case *TupleType:
			index, err := strconv.Atoi(expr.Key)
			if err != nil {
				return nil, nil, expr.WrapError(fmt.Errorf("cannot index tuple with %q", expr.Key))
			}

			if index >= len(typ.Elems()) {
				return nil, nil, expr.WrapError(fmt.Errorf("tuple index %d out bounds on tuple with %d elements", index, len(typ.Elems())))
			}

			return bc, receiverOp.Offset(typ.ElemOffset(index)), nil
		default:
			return nil, nil, expr.WrapError(fmt.Errorf("cannot dot index receiver type %T", expr.Receiver.Type()))
		}
	default:
		return nil, nil, fmt.Errorf("invalid lhs %T", expr)
	}
}

func (prog *Program) compileBCStatement(ctx context.Context, stmt Statement, scope *ValueScope) (BytecodeSnippet, error) {
	var bc BytecodeSnippet
	switch stmt := stmt.(type) {
	case *VarStatement:
		var err error
		var initBC []Bytecode
		var rhsOp *Operand

		local := scope.newLocal(stmt.Variable.Name(), stmt.Expression.Type())

		if stmt.Expression != nil {
			initBC, rhsOp, err = prog.compileBCExpression(ctx, stmt.Expression, scope, local)
			if err != nil {
				return nil, err
			}
		} else {
			initBC, rhsOp, err = prog.compileBCZeroValue(ctx, stmt.Type, scope, local)
			if err != nil {
				return nil, err
			}
		}

		bc.Add(initBC...)

		if rhsOp != local {
			bc.Add(Mov{
				Src:  rhsOp,
				Dst:  local,
				Size: stmt.Type.Size(),
			})
		}

		return bc, nil
	case *DeclarationStatement:
		local := scope.newLocal(stmt.Variable.Name(), stmt.Expression.Type())

		initBC, rhsOp, err := prog.compileBCExpression(ctx, stmt.Expression, scope, local)
		if err != nil {
			return nil, err
		}

		bc.Add(initBC...)

		if rhsOp != local {
			bc.Add(Mov{
				Src:  rhsOp,
				Dst:  local,
				Size: stmt.Expression.Type().Size(),
			})
		}

		return bc, nil
	case *AssignmentStatement:
		lhsBC, lhsOp, err := prog.compileBCLHS(ctx, stmt.Left, scope)
		if err != nil {
			return nil, err
		}

		rhsBC, rhsOp, err := prog.compileBCExpression(ctx, stmt.Right, scope, lhsOp)
		if err != nil {
			return nil, err
		}

		bc.Add(lhsBC...)
		bc.Add(rhsBC...)
		if lhsOp != rhsOp {
			bc.Add(Mov{
				Dst:  lhsOp,
				Src:  rhsOp,
				Size: stmt.Right.Type().Size(),
			})
		}

		return bc, nil
	case *AssignmentOperatorStatement:
		lhsBC, lhsOp, err := prog.compileBCLHS(ctx, stmt.Left, scope)
		if err != nil {
			return nil, err
		}

		rhsOp := scope.allocTemp(stmt.Right.Type())
		defer scope.deallocTemp(rhsOp)

		rhsBC, rhsOp, err := prog.compileBCExpression(ctx, stmt.Right, scope, rhsOp)
		if err != nil {
			return nil, err
		}

		bc.Add(lhsBC...)
		bc.Add(rhsBC...)

		var operator Operator
		switch stmt.Operator {
		case OperatorPlusEquals:
			operator = OperatorAddition
		case OperatorMinusEquals:
			operator = OperatorSubtraction
		case OperatorMultiplyEquals:
			operator = OperatorMultiplication
		case OperatorDivideEquals:
			operator = OperatorDivision
		default:
			return nil, stmt.WrapError(fmt.Errorf("unhandled assignment operator %q", stmt.Operator))
		}

		bc.Add(BinOp{
			Op:    BinaryOperation(stmt.Left.Type().Kind(), operator, stmt.Right.Type().Kind()),
			Dst:   lhsOp,
			Left:  lhsOp,
			Right: rhsOp,
		})

		return bc, nil
	case *PostfixStatement:
		lhsBC, lhsOp, err := prog.compileBCLHS(ctx, stmt.Expression, scope)
		if err != nil {
			return nil, err
		}

		bc.Add(lhsBC...)

		var operator Operator
		switch stmt.Operator {
		case OperatorIncrement:
			operator = OperatorAddition
		case OperatorSubtraction:
			operator = OperatorSubtraction
		default:
			return nil, stmt.WrapError(fmt.Errorf("unhandled postfix operator %q", stmt.Operator))
		}

		bc.Add(BinOp{
			Op:    BinaryOperation(stmt.Expression.Type().Kind(), operator, stmt.Expression.Type().Kind()),
			Dst:   lhsOp,
			Left:  lhsOp,
			Right: ImmediateOperand(Int(1)),
		})

		return bc, nil
	case *ReturnStatement:
		var offset AddrOffset
		offset += scope.function.Return().Size()
		for _, param := range scope.function.Parameters() {
			offset += param.Type().Size()
		}

		if scope.function.Return() != VoidType {
			var exprBC []Bytecode
			var err error

			returnVar, _ := scope.Get("__return")
			exprBC, exprOp, err := prog.compileBCExpression(ctx, stmt.Expression, scope, returnVar)
			if err != nil {
				return nil, err
			}

			bc.Add(exprBC...)

			if exprOp != returnVar {
				bc.Add(Mov{
					Dst:  returnVar,
					Src:  exprOp,
					Size: stmt.Expression.Type().Size(),
				})
			}
		}

		bc.Add(Return{
			Args: offset,
		})

		return bc, nil
	case *ForStatement:
		forScope := scope.sub(stmt.Scope)

		var err error
		var initBC []Bytecode
		if stmt.Init != nil {
			initBC, err = prog.compileBCStatement(ctx, stmt.Init, forScope)
			if err != nil {
				return nil, err
			}
		}

		var bodyBC []Bytecode
		for _, subStmt := range stmt.Body {
			bodyStmtBC, err := prog.compileBCStatement(ctx, subStmt, forScope)
			if err != nil {
				return nil, err
			}

			bodyBC = append(bodyBC, bodyStmtBC...)
		}

		var stepBC []Bytecode
		if stmt.Step != nil {
			stepBC, err = prog.compileBCStatement(ctx, stmt.Step, forScope)
			if err != nil {
				return nil, err
			}
		}

		var condBC BytecodeSnippet
		if stmt.Condition != nil {
			condTmp := forScope.allocTemp(stmt.Condition.Type())
			defer forScope.deallocTemp(condTmp)

			var condOp *Operand

			condBC, condOp, err = prog.compileBCExpression(ctx, stmt.Condition, forScope, condTmp)
			if err != nil {
				return nil, err
			}

			condBC.Add(JmpRC{
				Invert: true,
				Src:    condOp,
				Dst:    ImmediateOperand(Int(len(stepBC) + len(bodyBC) + 1)),
			})
		}

		bc.Add(initBC...)
		bc.Add(condBC...)
		bc.Add(bodyBC...)
		bc.Add(stepBC...)
		bc.Add(JmpR{
			Dst: ImmediateOperand(Int(-(len(bodyBC) + len(condBC) + len(stepBC) + 1))),
		})

		return bc, nil
	case *ExpressionStatement:
		if stmt.Expression.Type() != nil {
			tmp := scope.allocTemp(stmt.Expression.Type())
			defer scope.deallocTemp(tmp)

			exprBC, _, err := prog.compileBCExpression(ctx, stmt.Expression, scope, tmp)
			if err != nil {
				return nil, err
			}

			bc.Add(exprBC...)
		} else {
			exprBC, _, err := prog.compileBCExpression(ctx, stmt.Expression, scope, nil)
			if err != nil {
				return nil, err
			}

			bc.Add(exprBC...)
		}

		return bc, nil
	case *IfStatement:
		var condBC BytecodeSnippet
		var condOp *Operand

		if stmt.Condition != nil {
			condReg := scope.allocTemp(stmt.Condition.Type())

			var err error
			condBC, condOp, err = prog.compileBCExpression(ctx, stmt.Condition, scope, condReg)
			if err != nil {
				return nil, err
			}

			scope.deallocTemp(condReg)
		}

		bodyScope := scope.sub(stmt.Scope)

		var bodyBC []Bytecode
		for _, subStmt := range stmt.Body {
			bodyStmtBC, err := prog.compileBCStatement(ctx, subStmt, bodyScope)
			if err != nil {
				return nil, err
			}

			bodyBC = append(bodyBC, bodyStmtBC...)
		}

		if stmt.Condition != nil {
			condBC = append(condBC, JmpRC{
				Invert: true,
				Src:    condOp,
				Dst:    ImmediateOperand(Int(len(bodyBC))),
			})
		}

		bc.Add(condBC...)
		bc.Add(bodyBC...)

		if stmt.Else != nil {
			elseBC, err := prog.compileBCStatement(ctx, stmt.Else, scope)
			if err != nil {
				return nil, err
			}

			bc.Add(elseBC...)
		}

		return bc, nil
	default:
		return nil, stmt.WrapError(fmt.Errorf("unhandled statement %T in bytecode generator", stmt))
	}
}

func (p *Program) compileBCZeroValue(ctx context.Context, typ Type, scope *ValueScope, dst *Operand) (BytecodeSnippet, *Operand, error) {
	var bc BytecodeSnippet

	switch typ := BaseType(typ).(type) {
	case *BasicType:
		var imm Immediate
		switch typ.Kind() {
		case KindInt:
			imm = Int(0)
		case KindFloat:
			imm = Float(0)
		case KindString:
			imm = String("")
		case KindBool:
			imm = Bool(false)
		default:
			return nil, nil, fmt.Errorf("unhandled zero value for type %s", typ)
		}

		return nil, ImmediateOperand(imm), nil
	case *PointerType:
		return nil, ImmediateOperand(Int(0)), nil
	case *TupleType:
		var offset AddrOffset
		for _, elem := range typ.Elems() {
			dst := dst.Offset(offset)
			elemBC, elemOp, err := p.compileBCZeroValue(ctx, elem, scope, dst)
			if err != nil {
				return nil, nil, err
			}

			offset += elem.Size()

			bc.Add(elemBC...)

			if elemOp != dst {
				bc.Add(Mov{
					Src:  elemOp,
					Dst:  dst,
					Size: elem.Size(),
				})
			}
		}

		return bc, dst, nil
	default:
		return nil, nil, fmt.Errorf("unhandled zero value for type %s", typ)
	}
}

func (prog *Program) compileBCExpression(ctx context.Context, expr Expression, scope *ValueScope, dst *Operand) ([]Bytecode, *Operand, error) {
	var bc BytecodeSnippet
	switch expr := expr.(type) {
	case *Literal[Int]:
		return nil, ImmediateOperand(expr.Value()), nil
	case *Literal[Float]:
		return nil, ImmediateOperand(expr.Value()), nil
	case *Literal[String]:
		return nil, ImmediateOperand(expr.Value()), nil
	case *Literal[Bool]:
		return nil, ImmediateOperand(expr.Value()), nil
	case *OperandExpression:
		return nil, expr.Operand, nil
	case *SymbolReferenceExpression:
		sym, ok := scope.Get(expr.Name())
		if !ok {
			return nil, nil, expr.WrapError(fmt.Errorf("unknown symbol %q", expr.Name()))
		}
		return nil, sym, nil
	case *ParenthesizedExpression:
		return prog.compileBCExpression(ctx, expr.Expression, scope, dst)
	case *BinaryExpression:
		lhsBC, lhsOp, err := prog.compileBCExpression(ctx, expr.Left, scope, dst)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(lhsBC...)

		rhsReg := scope.allocTemp(expr.Right.Type())
		rhsBC, rhsOp, err := prog.compileBCExpression(ctx, expr.Right, scope, rhsReg)
		if err != nil {
			return nil, nil, err
		}
		defer scope.deallocTemp(rhsReg)

		bc.Add(rhsBC...)

		bc.Add(BinOp{
			Op:    BinaryOperation(expr.Left.Type().Kind(), expr.Operator, expr.Right.Type().Kind()),
			Dst:   dst,
			Left:  lhsOp,
			Right: rhsOp,
		})

		return bc, dst, nil
	case *UnaryExpression:
		switch expr.Operator {
		case OperatorAddress:
			// TODO: escape analysis
			switch subExpr := expr.Expression.(type) {
			case *SymbolReferenceExpression:
				sym, ok := scope.Get(subExpr.Name())
				if !ok {
					return nil, nil, fmt.Errorf("cannot take the address of unknown symbol %q", subExpr.Name())
				}

				bc = append(bc, LAddr{
					Dst: dst,
					Src: sym,
				})

				return bc, dst, nil
			default:
				return nil, nil, fmt.Errorf("address of non-symbol expressions is not currently supported")
			}

		default:

			srcBC, srcOp, err := prog.compileBCExpression(ctx, expr.Expression, scope, dst)
			if err != nil {
				return nil, nil, err
			}

			bc.Add(srcBC...)

			bc.Add(UnOp{
				Op:  UnaryOperation(expr.Operator, expr.Expression.Type().Kind()),
				Src: srcOp,
				Dst: dst,
			})

			return bc, dst, nil
		}
	case *CallExpression:
		callScope := scope.sub(scope.symbols)

		var offset AddrOffset
		ftype := expr.Function.Type().(*FunctionType)
		var retVar *Operand
		retVar = callScope.newArg(offset)
		offset += ftype.Return.Size()

		for _, arg := range expr.Args {
			argVar := callScope.newArg(offset)
			offset += arg.Type().Size()

			argBC, argOp, err := prog.compileBCExpression(ctx, arg, callScope, argVar)
			if err != nil {
				return nil, nil, err
			}

			bc.Add(argBC...)

			if argVar != argOp {
				bc.Add(Mov{
					Src:  argOp,
					Dst:  argVar,
					Size: arg.Type().Size(),
				})
			}
		}

		callReg := callScope.allocTemp(expr.Function.Type())
		defer callScope.deallocTemp(callReg)

		callBC, callOp, err := prog.compileBCExpression(ctx, expr.Function, callScope, callReg)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(callBC...)

		bc.Add(Call{
			Args: offset,
			Func: callOp,
		})

		if retVar != nil {
			bc.Add(Mov{
				Src:  retVar,
				Dst:  dst,
				Size: expr.Type().Size(),
			})
		}

		return bc, dst, nil
	case *DotExpression:
		receiverTmp := scope.allocTemp(expr.Receiver.Type())
		defer scope.deallocTemp(receiverTmp)
		receiverBC, receiverOp, err := prog.compileBCExpression(ctx, expr.Receiver, scope, receiverTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(receiverBC...)

		switch typ := BaseType(expr.Receiver.Type()).(type) {
		case *TupleType:
			index, err := strconv.Atoi(expr.Key)
			if err != nil {
				return nil, nil, expr.WrapError(fmt.Errorf("cannot index tuple with %q", expr.Key))
			}

			if index >= len(typ.Elems()) {
				return nil, nil, expr.WrapError(fmt.Errorf("tuple index %d out bounds on tuple with %d elements", index, len(typ.Elems())))
			}

			return bc, receiverOp.Offset(typ.ElemOffset(index)), nil
		default:
			return nil, nil, expr.WrapError(fmt.Errorf("cannot dot index receiver type %T", expr.Receiver.Type()))
		}
	default:
		return nil, nil, expr.WrapError(fmt.Errorf("unhandled expression in bytecode generator %T", expr))
	}
}

func (prog *Program) compileBCValuesLiteral(ctx context.Context, exprs []Expression, scope *ValueScope, dst *Operand) (BytecodeSnippet, error) {
	var bc BytecodeSnippet
	var offset AddrOffset
	for _, expr := range exprs {
		dst := dst.Offset(offset)
		exprBC, exprOp, err := prog.compileBCExpression(ctx, expr, scope, dst)
		if err != nil {
			return nil, err
		}

		bc.Add(exprBC...)
		if exprOp != dst {
			bc.Add(Mov{
				Src:  exprOp,
				Dst:  dst,
				Size: expr.Type().Size(),
			})
		}

		offset += expr.Type().Size()
	}

	return bc, nil
}
