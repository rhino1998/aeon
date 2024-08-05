package xenon

import (
	"fmt"
	"log"

	"github.com/rhino1998/aeon/pkg/compiler"
)

type compilerState struct {
	prog *compiler.Program

	regs          *RegisterState
	returnReg     *Operand
	constMap      map[string]map[string]Immediate
	globalMap     map[string]map[string]Addr
	externFuncMap map[string]*compiler.FunctionType
	funcMap       map[string]map[string]Addr
}

func Compile(prog *compiler.Program) ([]Bytecode, map[string]map[string]Addr, error) {
	cs := newCompilerState()
	bc, err := cs.compile(prog)
	if err != nil {
		return nil, nil, err
	}

	return bc, cs.funcMap, nil
}

func newCompilerState() *compilerState {
	regs := newRegisterState()
	return &compilerState{
		regs:          regs,
		returnReg:     regs.alloc(), // reserve reg 01 as the return register
		constMap:      make(map[string]map[string]Immediate),
		globalMap:     make(map[string]map[string]Addr),
		externFuncMap: make(map[string]*compiler.FunctionType),
		funcMap:       make(map[string]map[string]Addr),
	}
}

func (cs *compilerState) registerExternFunc(ftype *compiler.FunctionType) {
	cs.externFuncMap[ftype.Name()] = ftype
}

func (cs *compilerState) registerFunc(pkg, name string, addr Addr) {
	_, ok := cs.funcMap[pkg]
	if !ok {
		cs.funcMap[pkg] = make(map[string]Addr)
	}

	cs.funcMap[pkg][name] = addr
}

func (cs *compilerState) registerGlobal(pkg, name string, addr Addr) {
	_, ok := cs.globalMap[pkg]
	if !ok {
		cs.globalMap[pkg] = make(map[string]Addr)
	}

	cs.globalMap[pkg][name] = addr
}

func (cs *compilerState) registerConst(pkg, name string, value Immediate) {
	_, ok := cs.constMap[pkg]
	if !ok {
		cs.constMap[pkg] = make(map[string]Immediate)
	}

	cs.constMap[pkg][name] = value
}

func (cs *compilerState) compile(prog *compiler.Program) ([]Bytecode, error) {
	var bc []Bytecode
	for _, pkg := range prog.Packages() {
		// TODO: globals
		//
		// TODO: consts
		//
		var _ = pkg
	}

	for _, externFunc := range prog.ExternFuncs() {
		cs.registerExternFunc(externFunc)
	}

	for _, pkg := range prog.Packages() {
		for _, pkgFunc := range pkg.Functions() {
			funcAddr := Addr(len(bc))
			cs.registerFunc(pkg.Name(), pkgFunc.Name(), funcAddr)
			funcBC, err := cs.compileFunction(pkgFunc)
			if err != nil {
				return nil, err
			}

			bc = append(bc, funcBC...)
		}
	}

	return bc, nil
}

type Scope struct {
	varNames map[string]AddrOffset
	next     AddrOffset
}

func (r *Scope) setVar(name string, offset AddrOffset) {
	r.varNames[name] = offset
}

func (r *Scope) newVar(name string) Operand {
	r.varNames[name] = r.next
	r.next++
	return Operand{
		Value:  r.next - 1,
		Source: ValueSourceLocal,
	}
}

func (r *Scope) offset(name string) (*Operand, bool) {
	off, ok := r.varNames[name]
	if !ok {
		return nil, false
	}

	return &Operand{
		Value:  off,
		Source: ValueSourceLocal,
	}, true
}

func (r *Scope) sub() *Scope {
	varNames := make(map[string]AddrOffset)
	for name, offset := range r.varNames {
		varNames[name] = offset
	}

	return &Scope{
		varNames: varNames,
		next:     r.next,
	}
}

func newScope() *Scope {
	return &Scope{
		varNames: make(map[string]AddrOffset),
		next:     1,
	}
}

type RegisterState struct {
	registers map[Register]struct{}

	maxRegister Register
}

func newRegisterState() *RegisterState {
	regs := &RegisterState{
		registers: make(map[Register]struct{}),
	}

	return regs
}

func (r *RegisterState) save() []*Operand {
	ops := make([]*Operand, 0, r.maxRegister)
	log.Println(r.maxRegister)

	// skip reg0
	for i := Register(1); i <= r.maxRegister; i++ {
		if _, ok := r.registers[i]; !ok {
			continue
		}

		ops = append(ops, &Operand{
			Source: ValueSourceRegister,
			Value:  i,
		})
	}

	return ops
}

func (r *RegisterState) alloc() *Operand {
	var reg Register
	for {
		_, ok := r.registers[reg]
		if !ok {
			r.registers[reg] = struct{}{}
			if r.maxRegister < reg {
				r.maxRegister = reg
			}
			return &Operand{
				Source: ValueSourceRegister,
				Value:  reg,
			}
		}
		reg++
	}
}

func (r *RegisterState) dealloc(reg *Operand) {
	delete(r.registers, reg.Value.(Register))
}

func (cs *compilerState) compileFunction(f *compiler.Function) ([]Bytecode, error) {
	var bc []Bytecode

	scope := newScope()

	bc = append(bc, Push{
		Src: &Operand{
			Source: ValueSourceImmediate,
			Value:  String(fmt.Sprintf("func %s.%s", f.Package().Name(), f.Name())),
		},
	})
	_ = scope.newVar("#funcname")

	for i, param := range f.Parameters() {
		if param.Name() == "" {
			continue
		}

		scope.setVar(param.Name(), -AddrOffset(FrameSize+i+15))
	}

	for _, stmt := range f.Body() {
		stmtBC, err := cs.compileStatement(f, stmt, scope)
		if err != nil {
			return nil, err
		}

		bc = append(bc, stmtBC...)
	}

	// just in case
	bc = append(bc, Return{})
	return bc, nil
}

func (cs *compilerState) compileStatement(f *compiler.Function, stmt compiler.Statement, scope *Scope) ([]Bytecode, error) {
	var bc []Bytecode
	switch stmt := stmt.(type) {
	case *compiler.VarStatement:
		var err error
		var initBC []Bytecode

		reg := cs.regs.alloc()
		defer cs.regs.dealloc(reg)

		var rhsOp *Operand
		if stmt.Expression != nil {
			initBC, rhsOp, err = cs.compileExpression(stmt.Expression, scope, reg)
			if err != nil {
				return nil, err
			}
		} else {
			initBC, rhsOp, err = cs.makeZeroValue(stmt.Type, reg)
			if err != nil {
				return nil, err
			}
		}

		bc = append(bc, initBC...)
		bc = append(bc, Push{
			Src: rhsOp,
		})
		scope.newVar(stmt.Variable.Name())

		return bc, nil
	case *compiler.DeclarationStatement:
		reg := cs.regs.alloc()
		defer cs.regs.dealloc(reg)

		initBC, rhsOp, err := cs.compileExpression(stmt.Expression, scope, reg)
		if err != nil {
			return nil, err
		}

		bc = append(bc, initBC...)
		bc = append(bc, Push{
			Src: rhsOp,
		})
		scope.newVar(stmt.Variable.Name())

		return bc, nil
	case *compiler.AssignmentStatement:
		var lhsOp *Operand
		switch lhs := stmt.Left.(type) {
		case *compiler.SymbolReferenceExpression:
			op, ok := scope.offset(lhs.Name())
			if !ok {
				return nil, stmt.WrapError(fmt.Errorf("could not resolve name %q for assignment", lhs.Name()))
			}

			lhsOp = op
		default:
			return nil, stmt.WrapError(fmt.Errorf("unhandled LHS type in assigment: %T", lhs))
		}

		rhsBC, rhsOp, err := cs.compileExpression(stmt.Right, scope, lhsOp)
		if err != nil {
			return nil, err
		}

		bc = append(bc, rhsBC...)
		if lhsOp != rhsOp {
			bc = append(bc, Mov{
				Dst: lhsOp,
				Src: rhsOp,
			})
		}

		return bc, nil
	case *compiler.AssignmentOperatorStatement:
		var lhsOp *Operand
		// TODO: complex LHS
		switch lhs := stmt.Left.(type) {
		case *compiler.SymbolReferenceExpression:
			op, ok := scope.offset(lhs.Name())
			if !ok {
				return nil, stmt.WrapError(fmt.Errorf("could not resolve name %q for assignment", lhs.Name()))
			}

			lhsOp = op
		default:
			return nil, stmt.WrapError(fmt.Errorf("unhandled LHS type in assigment: %T", stmt.Left))
		}

		rhsOp := cs.regs.alloc()
		defer cs.regs.dealloc(rhsOp)

		rhsBC, rhsOp, err := cs.compileExpression(stmt.Right, scope, rhsOp)
		if err != nil {
			return nil, err
		}

		bc = append(bc, rhsBC...)

		var operator compiler.Operator
		switch stmt.Operator {
		case compiler.OperatorPlusEquals:
			operator = compiler.OperatorAddition
		case compiler.OperatorMinusEquals:
			operator = compiler.OperatorSubtraction
		case compiler.OperatorMultiplyEquals:
			operator = compiler.OperatorMultiplication
		case compiler.OperatorDivideEquals:
			operator = compiler.OperatorDivision
		default:
			return nil, stmt.WrapError(fmt.Errorf("unhandled assignment operator %q", stmt.Operator))
		}

		bc = append(bc, BinOp{
			Op:    BinaryOperator(stmt.Left.Type().Kind(), operator, stmt.Right.Type().Kind()),
			Dst:   lhsOp,
			Left:  lhsOp,
			Right: rhsOp,
		})

		return bc, nil
	case *compiler.PostfixStatement:
		var lhsOp *Operand
		// TODO: complex LHS
		switch lhs := stmt.Expression.(type) {
		case *compiler.SymbolReferenceExpression:
			op, ok := scope.offset(lhs.Name())
			if !ok {
				return nil, stmt.WrapError(fmt.Errorf("could not resolve name %q for assignment", lhs.Name()))
			}

			lhsOp = op
		default:
			return nil, stmt.WrapError(fmt.Errorf("unhandled LHS type in assigment: %T", stmt.Expression))
		}

		var operator compiler.Operator
		switch stmt.Operator {
		case compiler.OperatorIncrement:
			operator = compiler.OperatorAddition
		case compiler.OperatorSubtraction:
			operator = compiler.OperatorSubtraction
		default:
			return nil, stmt.WrapError(fmt.Errorf("unhandled postfix operator %q", stmt.Operator))
		}

		bc = append(bc, BinOp{
			Op:    BinaryOperator(stmt.Expression.Type().Kind(), operator, stmt.Expression.Type().Kind()),
			Dst:   lhsOp,
			Left:  lhsOp,
			Right: ImmediateOperand(Int(1)),
		})

		return bc, nil
	case *compiler.ReturnStatement:
		if f.Return() != nil {
			var exprBC []Bytecode
			var err error
			exprBC, rhsOp, err := cs.compileExpression(stmt.Expression, scope, cs.returnReg)
			if err != nil {
				return nil, err
			}
			bc = append(bc, exprBC...)

			if rhsOp != cs.returnReg {
				bc = append(bc, Mov{
					Dst: cs.returnReg,
					Src: rhsOp,
				})
			}
		}

		bc = append(bc, Return{})

		return bc, nil
	case *compiler.ForStatement:
		initScope := scope.sub()

		var err error
		var initBC []Bytecode
		if stmt.Init != nil {
			initBC, err = cs.compileStatement(f, stmt.Init, initScope)
			if err != nil {
				return nil, err
			}
		}

		bodyScope := initScope.sub()

		var bodyBC []Bytecode
		for _, subStmt := range stmt.Body {
			bodyStmtBC, err := cs.compileStatement(f, subStmt, bodyScope)
			if err != nil {
				return nil, err
			}

			bodyBC = append(bodyBC, bodyStmtBC...)
		}

		stepScope := initScope.sub()

		var stepBC []Bytecode
		if stmt.Step != nil {
			// variables defined in the step aren't in any real scope
			stepBC, err = cs.compileStatement(f, stmt.Step, stepScope)
			if err != nil {
				return nil, err
			}
		}

		var condBC []Bytecode
		if stmt.Condition != nil {
			condReg := cs.regs.alloc()
			defer cs.regs.dealloc(condReg)

			condBC, condReg, err = cs.compileExpression(stmt.Condition, initScope, condReg)
			if err != nil {
				return nil, err
			}

			condBC = append(condBC, JmpRC{
				Invert: true,
				Src:    condReg,
				Dst:    ImmediateOperand(Int(len(stepBC) + len(bodyBC) + 2)),
			})
		}

		bc = append(bc, initBC...)
		bc = append(bc, condBC...)
		bc = append(bc, bodyBC...)
		bc = append(bc, stepBC...)
		bc = append(bc, PopN{
			Src: ImmediateOperand(Int(max(
				stepScope.next-initScope.next,
				bodyScope.next-initScope.next,
			))),
		})
		bc = append(bc, JmpR{
			Dst: ImmediateOperand(Int(-(len(bodyBC) + len(condBC) + len(stepBC) + 2))),
		})
		bc = append(bc, PopN{
			Src: ImmediateOperand(Int(initScope.next - scope.next)),
		})

		return bc, nil
	case *compiler.ExpressionStatement:
		reg := cs.regs.alloc()
		defer cs.regs.dealloc(reg)

		exprBC, _, err := cs.compileExpression(stmt.Expression, scope, reg)
		if err != nil {
			return nil, err
		}

		bc = append(bc, exprBC...)

		return bc, nil
	case *compiler.IfStatement:
		condReg := cs.regs.alloc()

		condBC, condOp, err := cs.compileExpression(stmt.Condition, scope, condReg)
		if err != nil {
			return nil, err
		}

		cs.regs.dealloc(condReg)

		bodyScope := scope.sub()

		var bodyBC []Bytecode
		for _, subStmt := range stmt.Body {
			bodyStmtBC, err := cs.compileStatement(f, subStmt, bodyScope)
			if err != nil {
				return nil, err
			}

			bodyBC = append(bodyBC, bodyStmtBC...)
			bodyBC = append(bodyBC, PopN{
				Src: ImmediateOperand(Int(bodyScope.next - scope.next)),
			})
		}

		condBC = append(condBC, JmpRC{
			Invert: true,
			Src:    condOp,
			Dst:    ImmediateOperand(Int(len(bodyBC))),
		})

		bc = append(bc, condBC...)
		bc = append(bc, bodyBC...)

		if stmt.Else != nil {
			elseBC, err := cs.compileStatement(f, stmt.Else, scope)
			if err != nil {
				return nil, err
			}

			bc = append(bc, elseBC...)
		}

		return bc, nil
	case *compiler.ElseIfStatement:
		condReg := cs.regs.alloc()

		condBC, condOp, err := cs.compileExpression(stmt.Condition, scope, condReg)
		if err != nil {
			return nil, err
		}

		cs.regs.dealloc(condReg)

		bodyScope := scope.sub()

		var bodyBC []Bytecode
		for _, subStmt := range stmt.Body {
			bodyStmtBC, err := cs.compileStatement(f, subStmt, bodyScope)
			if err != nil {
				return nil, err
			}

			bodyBC = append(bodyBC, bodyStmtBC...)
			bodyBC = append(bodyBC, PopN{
				Src: ImmediateOperand(Int(bodyScope.next - scope.next)),
			})
		}

		condBC = append(condBC, JmpRC{
			Invert: true,
			Src:    condOp,
			Dst:    ImmediateOperand(Int(len(bodyBC))),
		})

		bc = append(bc, condBC...)
		bc = append(bc, bodyBC...)

		if stmt.Else != nil {
			elseBC, err := cs.compileStatement(f, stmt.Else, scope)
			if err != nil {
				return nil, err
			}

			bc = append(bc, elseBC...)
		}

		return bc, nil
	case *compiler.ElseStatement:
		condReg := cs.regs.alloc()
		defer cs.regs.dealloc(condReg)

		bodyScope := scope.sub()

		var bodyBC []Bytecode
		for _, subStmt := range stmt.Body {
			bodyStmtBC, err := cs.compileStatement(f, subStmt, bodyScope)
			if err != nil {
				return nil, err
			}

			bodyBC = append(bodyBC, bodyStmtBC...)
			bodyBC = append(bodyBC, PopN{
				Src: ImmediateOperand(Int(bodyScope.next - scope.next)),
			})
		}

		bc = append(bc, bodyBC...)

		return bc, nil
	default:
		return nil, stmt.WrapError(fmt.Errorf("unhandled statement %T in bytecode generator", stmt))
	}
}

func (cs *compilerState) makeZeroValue(typ compiler.Type, dst *Operand) ([]Bytecode, *Operand, error) {
	var bc []Bytecode
	switch typ := compiler.BaseType(typ).(type) {
	case *compiler.BasicType:
		var imm Immediate
		switch typ.Kind() {
		case compiler.KindInt:
			imm = Int(0)
		case compiler.KindFloat:
			imm = Float(0)
		case compiler.KindString:
			imm = String("")
		case compiler.KindBool:
			imm = Bool(false)
		default:
			return nil, nil, fmt.Errorf("unhandled zero value for type %s", typ)
		}

		return nil, ImmediateOperand(imm), nil
	case *compiler.PointerType:
		return nil, ImmediateOperand(Addr(0)), nil
	case *compiler.TupleType:
		elems := typ.Elems()
		bc = append(bc, Make{
			Kind: shortKind(compiler.KindTuple),
			Size: ImmediateOperand(Int(len(elems))),
			Dst:  dst,
		})

		for i, elem := range elems {
			elemReg := cs.regs.alloc()
			elemBC, elemDst, err := cs.makeZeroValue(elem, elemReg)
			if err != nil {
				return nil, nil, fmt.Errorf("tuple index %d: ", i)
			}

			bc = append(bc, elemBC...)

			bc = append(bc, SetIndex{
				Kind:  shortKind(compiler.KindTuple),
				Base:  dst,
				Index: ImmediateOperand(Int(i)),
				Src:   elemDst,
			})

			cs.regs.dealloc(elemReg)
		}

		return bc, dst, nil
	default:
		return nil, nil, fmt.Errorf("unhandled zero value for type %s", typ)
	}
}

func (cs *compilerState) compileExpression(expr compiler.Expression, scope *Scope, dst *Operand) ([]Bytecode, *Operand, error) {
	var bc []Bytecode
	switch expr := expr.(type) {
	case *compiler.Literal[int64]:
		return nil, ImmediateOperand(Int(expr.Value())), nil
	case *compiler.Literal[float64]:
		return nil, ImmediateOperand(Float(expr.Value())), nil
	case *compiler.Literal[string]:
		return nil, ImmediateOperand(String(expr.Value())), nil
	case *compiler.Literal[bool]:
		return nil, ImmediateOperand(Bool(expr.Value())), nil
	case *compiler.SymbolReferenceExpression:
		op, ok := scope.offset(expr.Name())
		if ok {
			return nil, op, nil
		}

		// TODO: handle globals

		return nil, nil, expr.WrapError(fmt.Errorf("could not resolve name %q for assignment", expr.Name()))
	case *compiler.ParenthesizedExpression:
		return cs.compileExpression(expr.Expression, scope, dst)
	case *compiler.BinaryExpression:
		lhsBC, lhsOp, err := cs.compileExpression(expr.Left, scope, dst)
		if err != nil {
			return nil, nil, err
		}

		bc = append(bc, lhsBC...)

		rhsReg := cs.regs.alloc()
		rhsBC, rhsOp, err := cs.compileExpression(expr.Right, scope, rhsReg)
		if err != nil {
			return nil, nil, err
		}
		defer cs.regs.dealloc(rhsReg)

		bc = append(bc, rhsBC...)

		bc = append(bc, BinOp{
			Op:    BinaryOperator(expr.Left.Type().Kind(), expr.Operator, expr.Right.Type().Kind()),
			Dst:   dst,
			Left:  lhsOp,
			Right: rhsOp,
		})

		return bc, dst, nil
	case *compiler.UnaryExpression:
		srcBC, srcOp, err := cs.compileExpression(expr.Expression, scope, dst)
		if err != nil {
			return nil, nil, err
		}

		bc = append(bc, srcBC...)

		switch expr.Operator {
		case compiler.OperatorAddress:
			// TODO: escape analysis
			switch subExpr := expr.Expression.(type) {
			case *compiler.SymbolReferenceExpression:
				offset, ok := scope.offset(subExpr.Name())
				if !ok {
					// TODO: globals
					return nil, nil, expr.WrapError(fmt.Errorf("could not resolve address of %v", subExpr.Name()))
				}

				bc = append(bc, LAddr{
					Dst: dst,
					Src: ImmediateOperand(Int(offset.Value.(AddrOffset))),
				})

				return bc, dst, nil
			default:
				return nil, nil, fmt.Errorf("address of non-symbol expressions is not currently supported")
			}

		default:
			bc = append(bc, UnOp{
				Op:  UnaryOperator(expr.Operator, expr.Expression.Type().Kind()),
				Dst: dst,
				Src: srcOp,
			})
		}

		return bc, dst, nil
	case *compiler.CallExpression:
		callScope := scope.sub()

		argReg := cs.regs.alloc()
		for i, arg := range expr.Args {
			argBC, argOp, err := cs.compileExpression(arg, callScope, argReg)
			if err != nil {
				return nil, nil, err
			}

			bc = append(bc, argBC...)
			bc = append(bc, Push{
				Src: argOp,
			})
			callScope.newVar(fmt.Sprintf("__arg%d", i))
		}
		cs.regs.dealloc(argReg)

		callBC, err := cs.compileFunctionCall(expr.Function, scope)
		if err != nil {
			return nil, nil, err
		}

		bc = append(bc, callBC...)

		bc = append(bc, Mov{
			Src: cs.returnReg,
			Dst: dst,
		})

		return bc, dst, nil
	default:
		return nil, nil, expr.WrapError(fmt.Errorf("unhandled expression in bytecode generator %T", expr))
	}
}

func (cs *compilerState) compileFunctionCall(expr compiler.Expression, scope *Scope) ([]Bytecode, error) {
	var bc []Bytecode

	switch expr := expr.(type) {
	case *compiler.ParenthesizedExpression:
		return cs.compileFunctionCall(expr.Expression, scope)
	case *compiler.SymbolReferenceExpression:
		funcAddr, ok := cs.funcMap["main"][expr.Name()]
		if ok {
			// static function call
			bc = append(bc, Call{
				Func: ImmediateOperand(Int(funcAddr)),
			})

			ftype := compiler.BaseType(expr.Type()).(*compiler.FunctionType)

			bc = append(bc, PopN{
				Src: ImmediateOperand(Int(len(ftype.Parameters))),
			})

			return bc, nil
		}

		if _, ok := cs.externFuncMap[expr.Name()]; ok {
			// extern function call

			bc = append(bc, CallExtern{
				Func: expr.Name(),
			})

			return bc, nil
		}

		// TODO: dynamic function calls & closures

		return nil, fmt.Errorf("cannot use non-static function references")
	case *compiler.DotExpression:
		// TODO: methods

		return nil, fmt.Errorf("cannot use a %T as a function reference yet", expr)
	default:
		return nil, fmt.Errorf("cannot use a %T as a function reference yet", expr)
	}
}
