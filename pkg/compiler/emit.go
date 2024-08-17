package compiler

import (
	"context"
	"fmt"
	"log"
	"strconv"

	"github.com/rhino1998/aeon/pkg/parser"
)

func (prog *Program) compileBytecode(ctx context.Context) error {
	// TODO: sort by import orderings
	registerOperands := make([]*Operand, prog.Registers())

	for reg := range Register(prog.Registers()) {
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

	for i, bc := range prog.bytecode {
		log.Printf("%02x: %v", i, bc)
	}

	return nil
}

func (pkg *Package) compileBytecode(ctx context.Context) error {
	pkg.values = BuiltinValues(pkg.prog.Registers(), pkg.scope)

	// TODO: sort topologically
	for _, constant := range pkg.Constants() {
		val, err := constant.Evaluate()
		if err != nil {
			return err
		}

		pkg.values.newConstant(constant.Name(), constant.Type(), val.Location(pkg.values).Operand)
	}

	varInitFunc, err := pkg.compileVarInit(ctx, pkg.values)
	if err != nil {
		return err
	}

	pkg.bytecode.Mount(varInitFunc)

	for _, fun := range pkg.Functions() {
		err := fun.compileBytecode(ctx, pkg.values)
		if err != nil {
			return err
		}

		pkg.bytecode.Mount(fun)
	}

	for _, drv := range pkg.DerivedTypes() {
		for _, met := range drv.MethodFunctions() {
			err := met.compileBytecode(ctx, pkg.values)
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

func (pkg *Package) compileVarInit(ctx context.Context, scope *ValueScope) (*Function, error) {
	f := newFunction(VarInitFuncName, pkg)
	f.receiver = &Variable{typ: VoidType}

	scope = scope.fun(pkg.scope)

	numLocals := ImmediateOperand(Int(0))

	f.bytecode.Add(BinOp{
		Op:    BinaryOperation(KindInt, OperatorAddition, KindInt),
		Dst:   OperandRegisterSP,
		Left:  OperandRegisterSP,
		Right: numLocals,
	})

	for _, externFunc := range pkg.ExternFunctions() {
		ptr := scope.newGlobal(externFunc.Name(), externType)
		hdrBC, err := pkg.prog.compileBCValuesLiteral(ctx, []Expression{
			NewLiteral(Int(1)),
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
		fName := fun.Name()
		log.Println(fName)
		ptr := scope.newGlobal(fName, funcType)
		hdrBC, err := pkg.prog.compileBCValuesLiteral(ctx, []Expression{
			NewLiteral(Int(2)),
			NewLiteral(String(fun.pkg.qualifiedName)),
			NewLiteral(Int(0)),
			&CompilerFunctionReferenceExpression{fun},
		}, scope, ptr)
		if err != nil {
			return nil, err
		}

		f.bytecode.Add(hdrBC...)

		fun.SetInfoAddr(Addr(ptr.AddressOf().Value.(Int)))
	}

	for _, global := range pkg.Globals() {
		ptr := scope.newGlobal(global.Name(), global.Type())
		if global.expr != nil {
			exprBC, exprLoc, err := pkg.prog.compileBCExpression(ctx, global.expr, scope, ptr)
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
			exprBC, exprLoc, err := pkg.prog.compileBCZeroValue(ctx, global.Position, global.Type(), scope, ptr)
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
			ptr := scope.newGlobal(met.Name(), funcType)
			hdrBC, err := pkg.prog.compileBCValuesLiteral(ctx, []Expression{
				NewLiteral(Int(2)),
				NewLiteral(String(met.QualifiedName())),
				NewLiteral(Int(0)),
				&CompilerFunctionReferenceExpression{met},
			}, scope, ptr)
			if err != nil {
				return nil, err
			}

			f.bytecode.Add(hdrBC...)

			met.SetInfoAddr(Addr(ptr.AddressOf().Value.(Int)))
		}
	}

	f.bytecode.Add(Return{})

	numLocals.Value = Int(*scope.maxLocal)

	pkg.varinit = f

	return f, nil
}

func (f *Function) compileBytecode(ctx context.Context, scope *ValueScope) error {
	scope = scope.fun(f.symbols)

	numLocals := ImmediateOperand(Int(0))

	f.bytecode.Add(Mov{
		Dst:  OperandRegisterSP,
		Src:  OperandRegisterSP.Offset(numLocals),
		Size: 1,
	})
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
		bcs, err := f.pkg.prog.compileBCStatement(ctx, stmt, scope)
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
	if _, ok := last.(*ReturnStatement); !ok && f.Return() == VoidType {
		f.bytecode.Add(Return{Args: argReturnSize})
	}

	// Can resolve here for debugging
	// err := f.bytecode.ResolveLabels()
	// if err != nil {
	// 	return err
	// }

	numLocals.Value = Int(*scope.maxLocal)

	return nil
}

func (prog *Program) compileBCGlobal(ctx context.Context, name string, expr Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, error) {
	var bc BytecodeSnippet
	globBC, globLoc, err := prog.compileBCExpression(ctx, expr, scope, dst)
	if err != nil {
		return nil, err
	}

	bc.Add(globBC...)

	if globLoc != dst {
		bc.Add(scope.Mov(globLoc, dst))
	}

	return bc, nil
}

func (prog *Program) compileBCLHS(ctx context.Context, expr Expression, scope *ValueScope) (BytecodeSnippet, *Location, error) {
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
		receiverBC, receiverLoc, err := prog.compileBCExpression(ctx, expr.Receiver, scope, receiverTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(receiverBC...)

		lhsBC, lhsLoc, err := prog.compileBCLHSDotExpression(ctx, expr, expr.Receiver.Type(), scope, receiverLoc)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(lhsBC...)

		return bc, lhsLoc, nil
	case *IndexExpression:
		receiverTmp := scope.allocTemp(expr.Receiver.Type())
		defer scope.deallocTemp(receiverTmp)
		receiverBC, receiverLoc, err := prog.compileBCExpression(ctx, expr.Receiver, scope, receiverTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(receiverBC...)

		indexTmp := scope.allocTemp(expr.Index.Type())
		defer scope.deallocTemp(indexTmp)
		indexBC, indexLoc, err := prog.compileBCExpression(ctx, expr.Index, scope, indexTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(indexBC...)

		lhsBC, lhsLoc, err := prog.compileBCLHSIndexExpression(ctx, expr, expr.Receiver.Type(), scope, receiverLoc, indexLoc)
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
			subExprBC, subExprLoc, err := prog.compileBCExpression(ctx, expr.Expression, scope, tmp)
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

func (prog *Program) compileBCLHSDotExpression(ctx context.Context, expr *DotExpression, typ Type, scope *ValueScope, receiverLoc *Location) (BytecodeSnippet, *Location, error) {
	switch typ := BaseType(typ).(type) {
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

		return prog.compileBCLHSDotExpression(ctx, expr, typ.Pointee(), scope, receiverLocValue)
	default:
		return nil, nil, expr.WrapError(fmt.Errorf("cannot dot index receiver type %s", resolveType(expr.Receiver.Type())))
	}
}

func (prog *Program) compileBCLHSIndexExpression(ctx context.Context, expr *IndexExpression, typ Type, scope *ValueScope, receiverLoc, indexLoc *Location) (BytecodeSnippet, *Location, error) {
	switch BaseType(typ).(type) {
	case *ArrayType:
		elemLoc, err := receiverLoc.IndexArray(indexLoc)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return nil, elemLoc, nil
	case *SliceType:
		// TODO:
		return nil, nil, expr.WrapError(fmt.Errorf("slice type not yet implemented"))
	case *MapType:
		// TODO:
		return nil, nil, expr.WrapError(fmt.Errorf("map type not yet implemented"))
	default:
		return nil, nil, expr.WrapError(fmt.Errorf("cannot index receiver type %s", expr.Receiver.Type()))
	}
}

func (prog *Program) compileBCStatement(ctx context.Context, stmt Statement, scope *ValueScope) (BytecodeSnippet, error) {
	var bc BytecodeSnippet
	switch stmt := stmt.(type) {
	case *VarStatement:
		var err error
		var initBC []Bytecode
		var rhsLoc *Location

		local := scope.newLocal(stmt.Variable.Name(), stmt.Type)

		if stmt.Expression != nil {
			initBC, rhsLoc, err = prog.compileBCExpression(ctx, stmt.Expression, scope, local)
			if err != nil {
				return nil, err
			}
		} else {
			initBC, rhsLoc, err = prog.compileBCZeroValue(ctx, stmt.Position, stmt.Type, scope, local)
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

		initBC, rhsOp, err := prog.compileBCExpression(ctx, stmt.Expression, scope, local)
		if err != nil {
			return nil, err
		}

		bc.Add(initBC...)

		if rhsOp != local {
			bc.Add(scope.Mov(rhsOp, local))
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
			bc.Add(scope.Mov(rhsOp, lhsOp))
		}

		return bc, nil
	case *AssignmentOperatorStatement:
		lhsBC, lhsLoc, err := prog.compileBCLHS(ctx, stmt.Left, scope)
		if err != nil {
			return nil, err
		}

		rhsLoc := scope.allocTemp(stmt.Right.Type())
		defer scope.deallocTemp(rhsLoc)

		rhsBC, rhsLoc, err := prog.compileBCExpression(ctx, stmt.Right, scope, rhsLoc)
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

		bc.BinOp(lhsLoc, lhsLoc, operator, rhsLoc)

		return bc, nil
	case *PostfixStatement:
		lhsBC, lhsLoc, err := prog.compileBCLHS(ctx, stmt.Expression, scope)
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

		bc.BinOp(lhsLoc, lhsLoc, operator, scope.newImmediate(Int(1)))

		return bc, nil
	case *ReturnStatement:
		var offset Size
		offset += scope.function.Return().Size()
		offset += scope.function.Receiver().Type().Size()
		for _, param := range scope.function.Parameters() {
			offset += param.Type().Size()
		}

		if scope.function.Return() != VoidType {
			var exprBC []Bytecode
			var err error

			returnVar, _ := scope.Get("__return")
			exprBC, exprLoc, err := prog.compileBCExpression(ctx, stmt.Expression, scope, returnVar)
			if err != nil {
				return nil, err
			}

			bc.Add(exprBC...)

			if exprLoc != returnVar {
				bc.Mov(returnVar, exprLoc)
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

		var loopBC BytecodeSnippet

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

		end := newLabel()

		var condBC BytecodeSnippet
		if stmt.Condition != nil {
			condTmp := forScope.allocTemp(stmt.Condition.Type())
			defer forScope.deallocTemp(condTmp)

			var condLoc *Location

			condBC, condLoc, err = prog.compileBCExpression(ctx, stmt.Condition, forScope, condTmp)
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
		var condLoc *Location

		if stmt.Condition != nil {
			condReg := scope.allocTemp(stmt.Condition.Type())

			var err error
			condBC, condLoc, err = prog.compileBCExpression(ctx, stmt.Condition, scope, condReg)
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
			elseBC, err := prog.compileBCStatement(ctx, stmt.Else, scope)
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

func (p *Program) compileBCZeroValue(ctx context.Context, pos parser.Position, typ Type, scope *ValueScope, dst *Location) (BytecodeSnippet, *Location, error) {
	var bc BytecodeSnippet

	switch typ := BaseType(typ).(type) {
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

			fieldBC, fieldLoc, err := p.compileBCZeroValue(ctx, pos, field.Type, scope, fieldDst)
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

			elemBC, elemLoc, err := p.compileBCZeroValue(ctx, pos, elem, scope, elemDst)
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

			elemBC, elemLoc, err := p.compileBCZeroValue(ctx, pos, elem, scope, dst)
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
		valBC, err := p.compileBCValuesLiteral(ctx, []Expression{
			NewLiteral(NilType.GlobalName()),
			NewLiteral(NilValue),
		}, scope, dst.AsType(interfaceTuple))
		if err != nil {
			return nil, nil, err
		}

		bc.Add(valBC...)

		return bc, dst, nil
	case *FunctionType:
		return p.compileBCZeroValue(ctx, pos, funcType, scope, dst.AsType(funcType))
	default:
		return nil, nil, pos.WrapError(fmt.Errorf("unhandled zero value for type %s", typ))
	}
}

func (prog *Program) compileBCExpression(ctx context.Context, expr Expression, scope *ValueScope, dst *Location) ([]Bytecode, *Location, error) {
	var bc BytecodeSnippet
	switch expr := expr.(type) {
	case *Literal:
		switch expr.Type().Kind() {
		case KindNil:
			return prog.compileBCZeroValue(ctx, expr.Position, dst.Type, scope, dst)
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
		return prog.compileBCExpression(ctx, expr.Expression, scope, dst)
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

		lhsBC, lhsLoc, err := prog.compileBCExpression(ctx, expr.Left, scope, lhsTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(lhsBC...)

		rhsTmp := scope.allocTemp(expr.Right.Type())
		rhsBC, rhsLoc, err := prog.compileBCExpression(ctx, expr.Right, scope, rhsTmp)
		if err != nil {
			return nil, nil, err
		}
		defer scope.deallocTemp(rhsTmp)

		bc.Add(rhsBC...)

		if expr.Type().Kind().IsPrimitive() && expr.Left.Type().Kind().IsPrimitive() && expr.Right.Type().Kind().IsPrimitive() {
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

			srcBC, srcLoc, err := prog.compileBCExpression(ctx, expr.Expression, scope, tmp)
			if err != nil {
				return nil, nil, err
			}

			bc.Add(srcBC...)

			return bc, srcLoc.AddressOf(), nil
		case OperatorDereference:
			tmp := scope.allocTemp(expr.Expression.Type())
			defer scope.deallocTemp(tmp)

			srcBC, srcOp, err := prog.compileBCExpression(ctx, expr.Expression, scope, tmp)
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
			srcBC, srcOp, err := prog.compileBCExpression(ctx, expr.Expression, scope, dst)
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

		switch ftype := BaseType(expr.Function.Type()).(type) {
		case *FunctionType:
			callTmp := callScope.allocTemp(expr.Function.Type())
			defer callScope.deallocTemp(callTmp)

			retVar := callScope.newArg("__return", 0, ftype.Return)

			recvVar := callScope.newArg("__recv", 0, ftype.Receiver)

			log.Println(ftype.Return)
			if ftype.Return.Size() > 0 {
				bc.Mov(scope.SP(), scope.SP().AddConst(ftype.Return.Size()))
			}

			callBC, callLoc, err := prog.compileBCCallReceiverExpression(ctx, expr.Function, callScope, recvVar, callTmp)
			if err != nil {
				return nil, nil, err
			}

			bc.Add(callBC...)

			for i, arg := range expr.Args {
				argVar := callScope.newArg(fmt.Sprintf("%d", i), 0, ftype.Parameters[i])

				argBC, argLoc, err := prog.compileBCExpression(ctx, arg, callScope, argVar)
				if err != nil {
					return nil, nil, err
				}

				bc.Add(argBC...)

				if argVar != argLoc {
					bc.Mov(argVar, argLoc)
				}

				bc.Mov(scope.SP(), scope.SP().AddConst(arg.Type().Size()))
			}

			bc.Add(Call{
				Func: callLoc.Operand.AddressOf(),
			})

			if retVar.Type.Size() > 0 {
				bc.Mov(dst, retVar)
			}

			return bc, dst, nil
		case *TypeType:
			if ftype.Type.Kind() == expr.Args[0].Type().Kind() {
				argsBC, argsLoc, err := prog.compileBCExpression(ctx, expr.Args[0], scope, dst)
				if err != nil {
					return nil, nil, err
				}

				bc.Add(argsBC...)

				// basic type conversions are no-ops
				return bc, argsLoc, nil
			}
			// TODO
			return nil, nil, expr.WrapError(fmt.Errorf("type conversions not yet implemented"))
		default:
			return nil, nil, expr.WrapError(fmt.Errorf("cannot call non-function type %v", ftype))
		}
	case *DotExpression:
		receiverTmp := scope.allocTemp(expr.Receiver.Type())
		defer scope.deallocTemp(receiverTmp)
		receiverBC, receiverLoc, err := prog.compileBCExpression(ctx, expr.Receiver, scope, receiverTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(receiverBC...)

		return prog.compileBCDotExpression(ctx, expr, expr.Receiver.Type(), scope, receiverLoc)
	case *IndexExpression:
		receiverTmp := scope.allocTemp(expr.Receiver.Type())
		defer scope.deallocTemp(receiverTmp)
		receiverBC, receiverLoc, err := prog.compileBCExpression(ctx, expr.Receiver, scope, receiverTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(receiverBC...)

		indexTmp := scope.allocTemp(expr.Receiver.Type())
		defer scope.deallocTemp(indexTmp)
		indexBC, indexLoc, err := prog.compileBCExpression(ctx, expr.Index, scope, indexTmp)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(indexBC...)

		return prog.compileBCIndexExpression(ctx, expr, expr.Receiver.Type(), scope, receiverLoc, indexLoc)
	case *TupleExpression:
		for i, elem := range expr.Elems {
			dst, err := dst.IndexTuple(i)
			if err != nil {
				return nil, nil, expr.WrapError(err)
			}

			elemBC, elemLoc, err := prog.compileBCExpression(ctx, elem, scope, dst)
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

			elemBC, elemLoc, err := prog.compileBCExpression(ctx, elem, scope, dst)
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

		valBC, valOp, err := prog.compileBCExpression(ctx, expr.Expression, scope, valElem)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(valBC...)

		if valOp != valElem {
			bc.Mov(valElem, valOp)
		}

		return bc, dst, nil
	case *TypeExpression:
		return bc, scope.typeName(expr.typ), nil
	case *BuiltinExpression:
		switch expr.Name {
		case "len":
			if len(expr.Args) != 1 {
				return nil, nil, expr.WrapError(fmt.Errorf("builtin %q expects exactly 1 argument, got %d", expr.Name, len(expr.Args)))
			}

			switch arg := BaseType(expr.Args[0].Type()).(type) {
			case *ArrayType:
				return nil, scope.newImmediate(Int(arg.Length())), nil
			case *SliceType:
				return nil, nil, expr.WrapError(fmt.Errorf("slice len not yet implemented"))
			case *MapType:
				return nil, nil, expr.WrapError(fmt.Errorf("map len not yet implemented"))
			default:
				return nil, nil, expr.WrapError(fmt.Errorf("cannot get length of %s", expr.Args[0].Type()))
			}
		default:
			return nil, nil, expr.WrapError(fmt.Errorf("invalid builtin %q", expr.Name))
		}
	default:
		return nil, nil, expr.WrapError(fmt.Errorf("unhandled expression in bytecode generator %T", expr))
	}
}

func (prog *Program) compileBCCallReceiverExpression(ctx context.Context, expr Expression, scope *ValueScope, recvDst, callDst *Location) (BytecodeSnippet, *Location, error) {
	var bc BytecodeSnippet
	switch expr := expr.(type) {
	case *MethodExpression:
		recvBC, recvLoc, err := prog.compileBCExpression(ctx, expr.Receiver, scope, recvDst)
		if err != nil {
			return nil, nil, err
		}

		bc.Add(recvBC...)

		methodName := expr.Method.Name

		if expr.Receiver.Type().Kind() == KindInterface {
			iface := BaseType(expr.Receiver.Type()).(*InterfaceType)

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
		return prog.compileBCCallReceiverExpression(ctx, expr.Expression, scope, recvDst, callDst)
	default:
		return prog.compileBCExpression(ctx, expr, scope, callDst)
	}
}

func (prog *Program) compileBCDotExpression(ctx context.Context, expr *DotExpression, typ Type, scope *ValueScope, receiverLoc *Location) (BytecodeSnippet, *Location, error) {
	switch typ := BaseType(typ).(type) {
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

		return prog.compileBCDotExpression(ctx, expr, typ.Pointee(), scope, receiverLocValue)
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
		return nil, nil, expr.WrapError(fmt.Errorf("cannot dot index receiver type %s", resolveType(expr.Receiver.Type())))
	}
}

func (prog *Program) compileBCIndexExpression(ctx context.Context, expr *IndexExpression, typ Type, scope *ValueScope, receiverLoc, indexLoc *Location) (BytecodeSnippet, *Location, error) {
	switch BaseType(typ).(type) {
	case *ArrayType:
		elemLoc, err := receiverLoc.IndexArray(indexLoc)
		if err != nil {
			return nil, nil, expr.WrapError(err)
		}

		return nil, elemLoc, nil
	case *StructType:
		// TODO:
		return nil, nil, expr.WrapError(fmt.Errorf("map type not yet implemented"))
	case *MapType:
		// TODO:
		return nil, nil, expr.WrapError(fmt.Errorf("slice type not yet implemented"))
	default:
		return nil, nil, expr.WrapError(fmt.Errorf("cannot index receiver type %s", expr.Receiver.Type()))
	}
}

func (prog *Program) compileBCValuesLiteral(ctx context.Context, exprs []Expression, scope *ValueScope, dst *Location) (BytecodeSnippet, error) {
	var bc BytecodeSnippet
	var offset Size

	for i, expr := range exprs {
		elemDst, err := dst.IndexTuple(i)
		if err != nil {
			return nil, err
		}

		exprBC, exprOp, err := prog.compileBCExpression(ctx, expr, scope, elemDst)
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
