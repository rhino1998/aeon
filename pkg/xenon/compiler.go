package xenon

import (
	"fmt"
	"log"

	"github.com/rhino1998/aeon/pkg/compiler"
)

type compilerState struct {
	prog *compiler.Program

	regs      *RegisterState
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
	return &compilerState{
		regs:      newRegisterState(),
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

func (r *Scope) newVar(name string) AddrOffset {
	r.varNames[name] = r.next
	r.next++
	return r.next - 1
}

func (r *Scope) offset(name string) AddrOffset {
	return r.varNames[name]
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

	regs.alloc() // discard reg 00

	return regs
}

func (r *RegisterState) alloc() Register {
	var reg Register
	for {
		_, ok := r.registers[reg]
		if !ok {
			r.registers[reg] = struct{}{}
			if r.maxRegister < reg {
				r.maxRegister = reg
			}
			return reg
		}
		reg++
	}
}

func (r *RegisterState) dealloc(reg Register) {
	delete(r.registers, reg)
}

func (cs *compilerState) compileFunction(f *compiler.Function) ([]Bytecode, error) {
	var bc []Bytecode

	scope := newScope()

	bc = append(bc, Push{
		Src: Operand{
			Kind:   compiler.KindString,
			Source: ValueSourceImmediate,
			Value:  String(fmt.Sprintf("#func %s", f.Name())),
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
	bc = append(bc, Return{0})
	return bc, nil
}

func (cs *compilerState) compileStatement(f *compiler.Function, stmt compiler.Statement, scope *Scope) ([]Bytecode, error) {
	var bc []Bytecode
	switch stmt := stmt.(type) {
	case *compiler.VarStatement:
		var err error
		var reg Register
		var initBC []Bytecode

		if stmt.Expression != nil {
			initBC, reg, err = cs.compileExpression(stmt.Expression, scope)
			if err != nil {
				return nil, err
			}
		} else {
			initBC, reg, err = cs.makeZeroValue(stmt.Type)
			if err != nil {
				return nil, err
			}
			// TODO: zero values
		}

		defer cs.regs.dealloc(reg)

		bc = append(bc, initBC...)
		bc = append(bc, Mov{
			Dst: Operand{
				Kind:   stmt.Variable.Type().Kind(),
				Source: ValueSourceLocal,
				Value:  scope.newVar(stmt.Variable.Name()),
			},
			Src: Operand{
				Kind:   stmt.Variable.Type().Kind(),
				Source: ValueSourceRegister,
				Value:  reg,
			},
		})
		return bc, nil
	case *compiler.DeclarationStatement:
		initBC, reg, err := cs.compileExpression(stmt.Expression, scope)
		if err != nil {
			return nil, err
		}
		defer cs.regs.dealloc(reg)

		bc = append(bc, initBC...)
		bc = append(bc, Mov{
			Dst: Operand{
				Kind:   stmt.Variable.Type().Kind(),
				Source: ValueSourceLocal,
				Value:  scope.newVar(stmt.Variable.Name()),
			},
			Src: Operand{
				Kind:   stmt.Expression.Type().Kind(),
				Source: ValueSourceRegister,
				Value:  reg,
			},
		})
		return bc, nil
	case *compiler.AssignmentStatement:
		rhsBC, rhsReg, err := cs.compileExpression(stmt.Right, scope)
		if err != nil {
			return nil, err
		}
		defer cs.regs.dealloc(rhsReg)

		bc = append(bc, rhsBC...)
		// TODO: resolve nested LHS
		//
		switch lhs := stmt.Left.(type) {
		case *compiler.SymbolReferenceExpression:
			// TODO: handle globals
			bc = append(bc, Mov{
				Dst: Operand{
					Kind:   lhs.Type().Kind(),
					Source: ValueSourceLocal,
					Value:  scope.offset(lhs.Name()),
				},
				Src: Operand{
					Kind:   stmt.Right.Type().Kind(),
					Source: ValueSourceRegister,
					Value:  rhsReg,
				},
			})

			return bc, nil
		default:
			return nil, stmt.WrapError(fmt.Errorf("unhandled LHS type in assigment: %T", lhs))
		}
	case *compiler.AssignmentOperatorStatement:
		var lhsOffset AddrOffset
		// TODO: complex LHS
		switch expr := stmt.Left.(type) {
		case *compiler.SymbolReferenceExpression:
			lhsOffset = scope.offset(expr.Name())
		default:
			return nil, stmt.WrapError(fmt.Errorf("unhandled LHS type in assigment: %T", stmt.Left))
		}

		rhsBC, rhsReg, err := cs.compileExpression(stmt.Right, scope)
		if err != nil {
			return nil, err
		}
		defer cs.regs.dealloc(rhsReg)

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
			Op: operator,
			Dst: Operand{
				Kind:   stmt.Left.Type().Kind(),
				Source: ValueSourceLocal,
				Value:  lhsOffset,
			},
			Left: Operand{
				Kind:   stmt.Left.Type().Kind(),
				Source: ValueSourceLocal,
				Value:  lhsOffset,
			},
			Right: Operand{
				Kind:   stmt.Right.Type().Kind(),
				Source: ValueSourceRegister,
				Value:  rhsReg,
			},
		})

		return bc, nil
	case *compiler.PostfixStatement:
		var lhsOffset AddrOffset
		// TODO: complex LHS
		switch expr := stmt.Expression.(type) {
		case *compiler.SymbolReferenceExpression:
			lhsOffset = scope.offset(expr.Name())
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
			Op: operator,
			Dst: Operand{
				Kind:   stmt.Expression.Type().Kind(),
				Source: ValueSourceLocal,
				Value:  lhsOffset,
			},
			Left: Operand{
				Kind:   stmt.Expression.Type().Kind(),
				Source: ValueSourceLocal,
				Value:  lhsOffset,
			},
			Right: Operand{
				Kind:   stmt.Expression.Type().Kind(),
				Source: ValueSourceImmediate,
				Value:  Int(1),
			},
		})

		return bc, nil
	case *compiler.ReturnStatement:
		var reg Register
		if f.Return() != nil {
			var exprBC []Bytecode
			var err error
			exprBC, reg, err = cs.compileExpression(stmt.Expression, scope)
			if err != nil {
				return nil, err
			}
			defer cs.regs.dealloc(reg)

			bc = append(bc, exprBC...)
		}

		bc = append(bc, Return{
			Register: reg,
		})

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
			var condReg Register
			condBC, condReg, err = cs.compileExpression(stmt.Condition, scope)
			if err != nil {
				return nil, err
			}
			defer cs.regs.dealloc(condReg)

			condBC = append(condBC, JmpRC{
				Invert: true,
				Src: Operand{
					Kind:   compiler.KindBool,
					Source: ValueSourceRegister,
					Value:  condReg,
				},
				Dst: Operand{
					Kind:   compiler.KindInt,
					Source: ValueSourceImmediate,
					Value:  Int(len(stepBC) + len(bodyBC) + 1),
				},
			})
		}

		log.Println(initBC)
		bc = append(bc, initBC...)
		bc = append(bc, condBC...)
		bc = append(bc, bodyBC...)
		bc = append(bc, stepBC...)
		bc = append(bc, JmpR{
			Dst: Operand{
				Kind:   compiler.KindInt,
				Source: ValueSourceImmediate,
				Value:  Int(-(len(bodyBC) + len(condBC) + len(stepBC) + 1)),
			},
		})

		return bc, nil
	case *compiler.IfStatement:
		condBC, condReg, err := cs.compileExpression(stmt.Condition, scope)
		if err != nil {
			return nil, err
		}
		defer cs.regs.dealloc(condReg)

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
			Src: Operand{
				Kind:   compiler.KindBool,
				Source: ValueSourceRegister,
				Value:  condReg,
			},
			Dst: Operand{
				Kind:   compiler.KindInt,
				Source: ValueSourceImmediate,
				Value:  Int(len(bodyBC)),
			},
		})

		bc = append(bc, condBC...)
		bc = append(bc, bodyBC...)

		return bc, nil
	default:
		return nil, stmt.WrapError(fmt.Errorf("unhandled statement %T in bytecode generator", stmt))
	}
}

func (cs *compilerState) makeZeroValue(typ compiler.Type) ([]Bytecode, Register, error) {
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
			return nil, 0, fmt.Errorf("unhandled zero value for type %s", typ)
		}

		reg := cs.regs.alloc()
		bc = append(bc, Mov{
			Dst: Operand{
				Kind:   typ.Kind(),
				Source: ValueSourceRegister,
				Value:  reg,
			},
			Src: Operand{
				Kind:   typ.Kind(),
				Source: ValueSourceImmediate,
				Value:  imm,
			},
		})

		return bc, reg, nil
	case *compiler.TupleType:
		elems := typ.Elems()
		reg := cs.regs.alloc()
		bc = append(bc, MakeTupleR{
			Size: len(elems),
			Dst:  reg,
		})

		for i, elem := range elems {
			elemBC, elemReg, err := cs.makeZeroValue(elem)
			if err != nil {
				return nil, 0, fmt.Errorf("tuple index %d: ", i)
			}

			bc = append(bc, elemBC...)

			bc = append(bc, SetIndexTupleRIR{
				Base:  reg,
				Index: Int(i),
				Src:   elemReg,
			})

			cs.regs.dealloc(elemReg)
		}

		return bc, reg, nil
	default:
		return nil, 0, fmt.Errorf("unhandled zero value for type %s", typ)
	}
}

func (cs *compilerState) compileExpression(expr compiler.Expression, scope *Scope) ([]Bytecode, Register, error) {
	var bc []Bytecode
	switch expr := expr.(type) {
	case *compiler.Literal[int64]:
		reg := cs.regs.alloc()
		bc = append(bc, Mov{
			Dst: Operand{
				Kind:   expr.Type().Kind(),
				Source: ValueSourceRegister,
				Value:  reg,
			},
			Src: Operand{
				Kind:   expr.Type().Kind(),
				Source: ValueSourceImmediate,
				Value:  Int(expr.Value()),
			},
		})

		return bc, reg, nil
	case *compiler.Literal[float64]:
		reg := cs.regs.alloc()
		bc = append(bc, Mov{
			Dst: Operand{
				Kind:   expr.Type().Kind(),
				Source: ValueSourceRegister,
				Value:  reg,
			},
			Src: Operand{
				Kind:   expr.Type().Kind(),
				Source: ValueSourceImmediate,
				Value:  Float(expr.Value()),
			},
		})
		return bc, reg, nil
	case *compiler.Literal[string]:
		reg := cs.regs.alloc()
		bc = append(bc, Mov{
			Dst: Operand{
				Kind:   expr.Type().Kind(),
				Source: ValueSourceRegister,
				Value:  reg,
			},
			Src: Operand{
				Kind:   expr.Type().Kind(),
				Source: ValueSourceImmediate,
				Value:  String(expr.Value()),
			},
		})

		return bc, reg, nil
	case *compiler.Literal[bool]:
		reg := cs.regs.alloc()
		bc = append(bc, Mov{
			Dst: Operand{
				Kind:   expr.Type().Kind(),
				Source: ValueSourceRegister,
				Value:  reg,
			},
			Src: Operand{
				Kind:   expr.Type().Kind(),
				Source: ValueSourceImmediate,
				Value:  Bool(expr.Value()),
			},
		})

		return bc, reg, nil
	case *compiler.BinaryExpression:
		lhsBC, lhsReg, err := cs.compileExpression(expr.Left, scope)
		if err != nil {
			return nil, 0, err
		}
		defer cs.regs.dealloc(lhsReg)

		bc = append(bc, lhsBC...)

		rhsBC, rhsReg, err := cs.compileExpression(expr.Right, scope)
		if err != nil {
			return nil, 0, err
		}
		defer cs.regs.dealloc(rhsReg)

		bc = append(bc, rhsBC...)

		reg := cs.regs.alloc()

		bc = append(bc, BinOp{
			Op: expr.Operator,
			Dst: Operand{
				Kind:   expr.Type().Kind(),
				Source: ValueSourceRegister,
				Value:  reg,
			},
			Left: Operand{
				Kind:   expr.Left.Type().Kind(),
				Source: ValueSourceRegister,
				Value:  lhsReg,
			},
			Right: Operand{
				Kind:   expr.Right.Type().Kind(),
				Source: ValueSourceRegister,
				Value:  rhsReg,
			},
		})

		return bc, reg, nil
	case *compiler.SymbolReferenceExpression:
		// TODO: handle globals

		reg := cs.regs.alloc()

		bc = append(bc, Mov{
			Dst: Operand{
				Kind:   expr.Type().Kind(),
				Source: ValueSourceRegister,
				Value:  reg,
			},
			Src: Operand{
				Kind:   expr.Type().Kind(),
				Source: ValueSourceLocal,
				Value:  scope.offset(expr.Name()),
			},
		})

		return bc, reg, nil
	default:
		return nil, 0, expr.WrapError(fmt.Errorf("unhandled expression %T", expr))
	}
}
