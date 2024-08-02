package xenon

import (
	"fmt"

	"github.com/rhino1998/aeon/pkg/compiler"
)

type compilerState struct {
	prog *compiler.Program

	regs      *RegisterState
	returnReg Operand
	constMap  map[string]map[string]Immediate
	globalMap map[string]map[string]Addr
	funcMap   map[string]map[string]Addr
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
		regs:      regs,
		returnReg: regs.alloc(), // reserve reg 01 as the return register
		constMap:  make(map[string]map[string]Immediate),
		globalMap: make(map[string]map[string]Addr),
		funcMap:   make(map[string]map[string]Addr),
	}
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

func (r *Scope) offset(name string) Operand {
	return Operand{
		Value:  r.varNames[name],
		Source: ValueSourceLocal,
	}
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

func (r *RegisterState) alloc() Operand {
	var reg Register
	for {
		_, ok := r.registers[reg]
		if !ok {
			r.registers[reg] = struct{}{}
			if r.maxRegister < reg {
				r.maxRegister = reg
			}
			return Operand{
				Source: ValueSourceRegister,
				Value:  reg,
			}
		}
		reg++
	}
}

func (r *RegisterState) dealloc(reg Operand) {
	delete(r.registers, reg.Value.(Register))
}

func (cs *compilerState) compileFunction(f *compiler.Function) ([]Bytecode, error) {
	var bc []Bytecode

	scope := newScope()

	bc = append(bc, Push{
		Src: Operand{
			Source: ValueSourceImmediate,
			Value:  String(fmt.Sprintf("func %s.%s", f.Package().Name(), f.Name())),
		},
	})
	scope.newVar("#funcname")

	for i, param := range f.Parameters() {
		if param.Name() == "" {
			continue
		}

		scope.setVar(param.Name(), -AddrOffset(FrameSize+i))
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

		dst := scope.newVar(stmt.Variable.Name())

		var rhsOp Operand
		if stmt.Expression != nil {
			initBC, rhsOp, err = cs.compileExpression(stmt.Expression, scope, dst)
			if err != nil {
				return nil, err
			}
		} else {
			initBC, rhsOp, err = cs.makeZeroValue(stmt.Type, dst)
			if err != nil {
				return nil, err
			}
		}

		bc = append(bc, initBC...)
		if rhsOp != dst {
			bc = append(bc, Mov{
				Dst: dst,
				Src: rhsOp,
			})
		}

		return bc, nil
	case *compiler.DeclarationStatement:
		dst := scope.newVar(stmt.Variable.Name())
		initBC, rhsOp, err := cs.compileExpression(stmt.Expression, scope, dst)
		if err != nil {
			return nil, err
		}

		bc = append(bc, initBC...)

		if rhsOp != dst {
			bc = append(bc, Mov{
				Dst: dst,
				Src: rhsOp,
			})
		}

		return bc, nil
	case *compiler.AssignmentStatement:
		var lhsOp Operand
		switch lhs := stmt.Left.(type) {
		case *compiler.SymbolReferenceExpression:
			lhsOp = scope.offset(lhs.Name())
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
		var lhsOp Operand
		// TODO: complex LHS
		switch expr := stmt.Left.(type) {
		case *compiler.SymbolReferenceExpression:
			lhsOp = scope.offset(expr.Name())
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
		var lhsOp Operand
		// TODO: complex LHS
		switch expr := stmt.Expression.(type) {
		case *compiler.SymbolReferenceExpression:
			lhsOp = scope.offset(expr.Name())
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
		var err error
		var initBC []Bytecode
		if stmt.Init != nil {
			initBC, err = cs.compileStatement(f, stmt.Init, scope)
			if err != nil {
				return nil, err
			}
		}

		var stepBC []Bytecode
		if stmt.Step != nil {
			// variables defined in the step aren't in any real scope
			stepBC, err = cs.compileStatement(f, stmt.Step, scope.sub())
			if err != nil {
				return nil, err
			}
		}

		bodyScope := scope.sub()

		var bodyBC []Bytecode
		for _, subStmt := range stmt.Body {
			bodyStmtBC, err := cs.compileStatement(f, subStmt, bodyScope)
			if err != nil {
				return nil, err
			}

			bodyBC = append(bodyBC, bodyStmtBC...)
		}

		var condBC []Bytecode
		if stmt.Condition != nil {
			condReg := cs.regs.alloc()
			defer cs.regs.dealloc(condReg)

			condBC, condReg, err = cs.compileExpression(stmt.Condition, scope, condReg)
			if err != nil {
				return nil, err
			}

			condBC = append(condBC, JmpRC{
				Invert: true,
				Src:    condReg,
				Dst:    ImmediateOperand(Int(len(stepBC) + len(bodyBC) + 1)),
			})
		}

		bc = append(bc, initBC...)
		bc = append(bc, condBC...)
		bc = append(bc, bodyBC...)
		bc = append(bc, stepBC...)
		bc = append(bc, JmpR{
			Dst: ImmediateOperand(Int(-(len(bodyBC) + len(condBC) + len(stepBC) + 1))),
		})

		return bc, nil
	case *compiler.IfStatement:
		condReg := cs.regs.alloc()
		defer cs.regs.dealloc(condReg)

		condBC, condReg, err := cs.compileExpression(stmt.Condition, scope, condReg)
		if err != nil {
			return nil, err
		}

		bodyScope := scope.sub()

		var bodyBC []Bytecode
		for _, subStmt := range stmt.Body {
			bodyStmtBC, err := cs.compileStatement(f, subStmt, bodyScope)
			if err != nil {
				return nil, err
			}

			bodyBC = append(bodyBC, bodyStmtBC...)
		}

		condBC = append(condBC, JmpRC{
			Invert: true,
			Src:    condReg,
			Dst:    ImmediateOperand(Int(len(bodyBC))),
		})

		bc = append(bc, condBC...)
		bc = append(bc, bodyBC...)

		return bc, nil
	default:
		return nil, stmt.WrapError(fmt.Errorf("unhandled statement %T in bytecode generator", stmt))
	}
}

func (cs *compilerState) makeZeroValue(typ compiler.Type, dst Operand) ([]Bytecode, Operand, error) {
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
			return nil, Operand{}, fmt.Errorf("unhandled zero value for type %s", typ)
		}

		return nil, Operand{
			Source: ValueSourceImmediate,
			Value:  imm,
		}, nil
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
				return nil, Operand{}, fmt.Errorf("tuple index %d: ", i)
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
		return nil, Operand{}, fmt.Errorf("unhandled zero value for type %s", typ)
	}
}

func (cs *compilerState) compileExpression(expr compiler.Expression, scope *Scope, dst Operand) ([]Bytecode, Operand, error) {
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
		// TODO: handle globals
		return nil, scope.offset(expr.Name()), nil
	case *compiler.ParenthesizedExpression:
		return cs.compileExpression(expr.Expression, scope, dst)
	case *compiler.BinaryExpression:
		lhsBC, lhsOp, err := cs.compileExpression(expr.Left, scope, dst)
		if err != nil {
			return nil, Operand{}, err
		}

		bc = append(bc, lhsBC...)

		rhsReg := cs.regs.alloc()
		rhsBC, rhsOp, err := cs.compileExpression(expr.Right, scope, rhsReg)
		if err != nil {
			return nil, Operand{}, err
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
	default:
		return nil, Operand{}, expr.WrapError(fmt.Errorf("unhandled expression in bytecode generator %T", expr))
	}
}
