package xenon

import (
	"context"
	"fmt"
	"log"
	"math"
	"strings"

	"github.com/rhino1998/aeon/pkg/compiler"
)

const (
	RuntimeFuncTypeNil    = 0
	RuntimeFuncTypeExtern = 1
	RuntimeFuncTypeFunc   = 2
)

type RuntimeExternFuncEntry struct {
	ArgSize    Size
	ReturnSize Size
	Func       RuntimeExternFunc
}

func DefaultExternFuncs() RuntimeExternFuncs {
	return RuntimeExternFuncs{
		"__builtin_assert": {
			ArgSize:    2,
			ReturnSize: 0,
			Func: func(r *Runtime, s []float64) float64 {
				str, err := r.LoadString(Addr(s[1]))
				if err != nil {
					panic(err)
				}

				if s[0] == 0 {
					panic(fmt.Sprintf("assertion failed: %s", string(str)))
				}

				return 0
			},
		},
		"print": {
			ArgSize:    1,
			ReturnSize: 0,
			Func: func(r *Runtime, s []float64) float64 {
				str, err := r.LoadString(Addr(s[0]))
				if err != nil {
					panic(err)
				}
				fmt.Println(string(str))
				return 0
			},
		},
		"itoa": {
			ArgSize:    1,
			ReturnSize: 1,
			Func: func(r *Runtime, s []float64) float64 {
				addr, err := r.allocStr(String(fmt.Sprintf("%d", int(s[0]))))
				if err != nil {
					panic(err)
				}
				return float64(addr)
			},
		},
		"print3": {
			ArgSize:    3,
			ReturnSize: 0,
			Func: func(_ *Runtime, s []float64) float64 {
				fmt.Println(s)
				return 0
			},
		},
		"printInt": {
			ArgSize:    1,
			ReturnSize: 0,
			Func: func(_ *Runtime, s []float64) float64 {
				fmt.Println(s[0])
				return 0
			},
		},
		"panic": {
			ArgSize: 1,
			Func: func(_ *Runtime, s []float64) float64 {
				panic(s[0])
			},
		},
		"add": {
			ArgSize:    2,
			ReturnSize: 1,
			Func: func(r *Runtime, s []float64) float64 {
				return opAdd[Int](s[0], s[1])
			},
		},
	}
}

const PageSize = 65535 // annoyingly not a power of 2

type RuntimeExternFuncs map[String]RuntimeExternFuncEntry

type RuntimeExternFunc func(*Runtime, []float64) float64

type Runtime struct {
	prog        *compiler.Program
	externFuncs RuntimeExternFuncs

	debug bool

	codePages [][PageSize]compiler.Bytecode
	heapStart Addr
	heapIndex Addr

	registers []float64

	memPages [][PageSize]float64
	strIndex Addr
	strPages [][PageSize]String

	vtables map[int]map[string]float64
}

func NewRuntime(prog *compiler.Program, externs RuntimeExternFuncs, memPages, strPages int, debug bool) (*Runtime, error) {
	heapStart := Addr(memPages * PageSize / 2)

	r := &Runtime{
		prog:        prog,
		externFuncs: externs,

		debug: debug,

		codePages: make([][PageSize]compiler.Bytecode, (len(prog.Bytecode())+PageSize-1)/PageSize),
		memPages:  make([][PageSize]float64, memPages),
		strPages:  make([][PageSize]String, strPages),
		registers: make([]float64, prog.Registers()),

		heapStart: heapStart,
		heapIndex: heapStart,

		vtables: make(map[int]map[string]float64),
	}

	for i, str := range prog.Strings() {
		page, pageAddr := r.splitAddr(compiler.Addr(i))
		r.strPages[page][pageAddr] = str
		r.strIndex++
	}

	for i, code := range prog.Bytecode() {
		page, pageAddr := r.splitAddr(compiler.Addr(i))
		r.codePages[page][pageAddr] = code
	}

	for typeID, typ := range prog.Types() {
		r.vtables[typeID] = make(map[string]float64)
		if debug {
			log.Printf("type %v: %d", typ, typeID)
		}
		r.vtables[typeID]["#size"] = float64(typ.Size())
		r.vtables[typeID]["#kind"] = float64(typ.Kind())
		switch typ := typ.(type) {
		case *compiler.DerivedType:
			for _, method := range typ.Methods(false) {
				fun := typ.Method(method.Name, false)
				if fun == nil {
					return nil, fmt.Errorf("failed to resolve method %s on type %s", method.Name, typ)
				}
				r.vtables[typeID][method.Name] = float64(fun.InfoAddr())
			}
		case *compiler.PointerType:
			switch typ := typ.Pointee().(type) {
			case *compiler.DerivedType:
				for _, method := range typ.Methods(true) {
					fun := typ.Method(method.Name, true)
					if fun == nil {
						return nil, fmt.Errorf("failed to resolve method %s on type %s", method.Name, typ)
					}
					r.vtables[typeID][method.Name] = float64(fun.InfoAddr())
				}
			}
		}
	}

	return r, nil
}

func (r *Runtime) pc() compiler.Addr {
	return Addr(r.registers[compiler.RegisterPC])
}

func (r *Runtime) setPC(addr compiler.Addr) {
	r.registers[compiler.RegisterPC] = float64(addr)
}

func (r *Runtime) fp() compiler.Addr {
	return Addr(r.registers[compiler.RegisterFP])
}

func (r *Runtime) setFP(addr compiler.Addr) {
	r.registers[compiler.RegisterFP] = float64(addr)
}

func (r *Runtime) sp() compiler.Addr {
	return Addr(r.registers[compiler.RegisterSP])
}

func (r *Runtime) setSP(addr compiler.Addr) {
	r.registers[compiler.RegisterSP] = float64(addr)
}

func (r *Runtime) splitAddr(addr compiler.Addr) (uint64, uint64) {
	return uint64(addr / PageSize), uint64(addr % PageSize)
}

func (r *Runtime) fetch(addr compiler.Addr) (compiler.Bytecode, error) {
	page, pageAddr := r.splitAddr(addr)

	if page >= uint64(len(r.codePages)) {
		return nil, fmt.Errorf("invalid code page %v for addr %v", page, addr)
	}

	if pageAddr >= uint64(len(r.codePages[page])) {
		return nil, fmt.Errorf("invalid code page addr %v in page %v for addr %v", pageAddr, page, addr)
	}

	return r.codePages[page][pageAddr], nil
}

func (r *Runtime) loadAddr(addr compiler.Addr) (float64, error) {
	page, pageAddr := r.splitAddr(addr)

	if page >= uint64(len(r.memPages)) {
		return 0, fmt.Errorf("invalid page %v for addr %v", page, addr)
	}

	if pageAddr >= uint64(len(r.memPages[page])) {
		return 0, fmt.Errorf("invalid page addr %v in page %v for addr %v", pageAddr, page, addr)
	}

	return r.memPages[page][pageAddr], nil
}

func (r *Runtime) LoadString(addr compiler.Addr) (String, error) {
	page, pageAddr := r.splitAddr(addr)

	if page >= uint64(len(r.strPages)) {
		return "", fmt.Errorf("invalid str page %v for addr %v", page, addr)
	}

	if pageAddr >= uint64(len(r.strPages[page])) {
		return "", fmt.Errorf("invalid str page addr %v in page %v for addr %v", pageAddr, page, addr)
	}

	return r.strPages[page][pageAddr], nil
}

func (r *Runtime) loadArgs(sp compiler.Addr, size Size) ([]float64, error) {
	var args []float64
	for offset := Size(0); offset < size; offset++ {
		arg, err := r.loadAddr(sp.Offset(offset - size))
		if err != nil {
			return nil, err
		}

		args = append(args, arg)
	}

	return args, nil
}

func (r *Runtime) storeAddr(addr compiler.Addr, val float64) error {
	page, pageAddr := r.splitAddr(addr)

	if page >= uint64(len(r.memPages)) {
		return fmt.Errorf("invalid page %v for addr %v", page, addr)
	}

	if pageAddr >= uint64(len(r.memPages[page])) {
		return fmt.Errorf("invalid page addr %v in page %v for addr %v", pageAddr, page, addr)
	}

	r.memPages[page][pageAddr] = val

	return nil
}

func (r *Runtime) storeStr(addr compiler.Addr, val String) error {
	page, pageAddr := r.splitAddr(addr)

	if page >= uint64(len(r.strPages)) {
		return fmt.Errorf("invalid str page 0x%08x for addr 0x%08x", page, addr)
	}

	if pageAddr >= uint64(len(r.strPages[page])) {
		return fmt.Errorf("invalid str page addr 0x%08x in page 0x%08x for addr %s", pageAddr, page, addr)
	}

	r.strPages[page][pageAddr] = val

	return nil
}

func (r *Runtime) allocStr(val String) (Addr, error) {
	r.strIndex++
	page, pageAddr := r.splitAddr(r.strIndex)

	if page >= uint64(len(r.strPages)) {
		return 0, fmt.Errorf("invalid str page 0x%08x for addr 0x%08x", page, r.strIndex)
	}

	if pageAddr >= uint64(len(r.strPages[page])) {
		return 0, fmt.Errorf("invalid str page addr 0x%08x in page 0x%08x for addr %s", pageAddr, page, r.strIndex)
	}

	r.strPages[page][pageAddr] = val

	return r.strIndex, nil
}

func (r *Runtime) alloc(size Size) (Addr, error) {
	addr := r.heapIndex
	r.heapIndex += Addr(size)
	if r.heapIndex > Addr(len(r.memPages)*PageSize) {
		return 0, fmt.Errorf("out of memory")
	}

	// TODO: garbage collection

	return addr, nil
}

func (env *Runtime) push(val float64) error {
	err := env.storeAddr(env.sp(), val)
	env.setSP(env.sp() + 1)
	return err
}

func (r *Runtime) pop() (float64, error) {
	val, err := r.loadAddr(r.sp())
	r.setSP(r.sp() - 1)
	return val, err
}

func (r *Runtime) loadImmediate(imm compiler.Immediate) loadFunc {
	return loadFunc(func() (float64, error) {
		switch imm := imm.(type) {
		case Int:
			return float64(imm), nil
		case Float:
			return float64(imm), nil
		case Bool:
			if imm {
				return 1, nil
			}
			return 0, nil
		default:
			panic("BAD IMMEDIATE TYPE")
		}
	})
}

func (r *Runtime) loadIndirect(indirect compiler.Indirect) loadFunc {
	return r.loadIndirectWithOffset(indirect.Ptr, 0)
}

func (r *Runtime) loadBinary(binop compiler.BinaryOperand) loadFunc {
	return loadFunc(func() (float64, error) {
		a, err := r.load(binop.Left)()
		if err != nil {
			return 0, err
		}

		b, err := r.load(binop.Right)()
		if err != nil {
			return 0, err
		}

		switch binop.Op {
		case "+":
			return a + b, nil
		case "-":
			return a - b, nil
		case "*":
			return a * b, nil
		case "<":
			if a < b {
				return 1, nil
			} else {
				return 0, nil
			}
		case "<=":
			if a <= b {
				return 1, nil
			} else {
				return 0, nil
			}
		case "==":
			if a == b {
				return 1, nil
			} else {
				return 0, nil
			}
		case "!=":
			if a != b {
				return 1, nil
			} else {
				return 0, nil
			}
		case "#":
			if int(a) >= int(b) {
				return 0, fmt.Errorf("index out of bounds: %d >= %d", int(a), int(b))
			}

			return a, nil
		default:
			return 0, fmt.Errorf("unhandled binary operand %v", binop.Op)
		}
	})
}

func (r *Runtime) loadUnary(not compiler.UnaryOperand) loadFunc {
	return loadFunc(func() (float64, error) {
		a, err := r.load(not.A)()
		if err != nil {
			return 0, err
		}

		switch not.Op {
		case compiler.OperatorNot:
			if a == 0 {
				return 1, nil
			}

			return 0, nil
		default:
			return 0, fmt.Errorf("unhandled unary operator %v", not.Op)
		}

	})
}

func (r *Runtime) loadIndirectWithOffset(indirect *compiler.Operand, offset Size) loadFunc {
	return loadFunc(func() (float64, error) {
		base, err := r.load(indirect)()
		if err != nil {
			return 0, err
		}

		return r.loadAddr(Addr(base).Offset(offset))
	})
}

func (r *Runtime) loadRegister(reg compiler.Register) loadFunc {
	return loadFunc(func() (float64, error) {
		return r.registers[reg], nil
	})
}

func (r *Runtime) loadVTable(lookup compiler.VTableLookup) loadFunc {
	return loadFunc(func() (float64, error) {
		typeID, err := r.load(lookup.Type)()
		if err != nil {
			return 0, fmt.Errorf("failed to get type id: %w", err)
		}

		nameAddr, err := r.load(lookup.Method)()
		if err != nil {
			return 0, fmt.Errorf("failed to get method name addr: %w", err)
		}

		name, err := r.LoadString(Addr(nameAddr))
		if err != nil {
			return 0, fmt.Errorf("failed to get method name: %w", err)
		}

		funAddr, ok := r.vtables[int(typeID)][string(name)]
		if !ok {
			return 0, fmt.Errorf("no such vtable entry for %d %s", int(typeID), name)
		}

		return funAddr, nil
	})
}

func (r *Runtime) load(operand *compiler.Operand) loadFunc {
	switch operand.Kind {
	case compiler.OperandKindImmediate:
		return r.loadImmediate(operand.Value.(compiler.Immediate))
	case compiler.OperandKindIndirect:
		return r.loadIndirect(operand.Value.(compiler.Indirect))
	case compiler.OperandKindRegister:
		return r.loadRegister(operand.Value.(compiler.Register))
	case compiler.OperandKindBinary:
		return r.loadBinary(operand.Value.(compiler.BinaryOperand))
	case compiler.OperandKindUnary:
		return r.loadUnary(operand.Value.(compiler.UnaryOperand))
	case compiler.OperandKindVTableLookup:
		return r.loadVTable(operand.Value.(compiler.VTableLookup))
	default:
		return func() (float64, error) {
			return 0, fmt.Errorf("bad %s operand in runtime", operand.Kind)
		}
	}
}

func (r *Runtime) storeIndirect(indirect compiler.Indirect) storeFunc {
	return storeFunc(func(val float64, err error) error {
		if err != nil {
			return err
		}

		ptr, err := r.load(indirect.Ptr)()
		if err != nil {
			return err
		}

		return r.storeAddr(Addr(ptr), val)
	})
}

func (r *Runtime) storeRegister(reg compiler.Register) storeFunc {
	return storeFunc(func(val float64, err error) error {
		if err != nil {
			return err
		}

		r.registers[reg] = val
		return nil
	})
}

func (r *Runtime) store(operand *compiler.Operand) storeFunc {
	switch operand.Kind {
	case compiler.OperandKindIndirect:
		return r.storeIndirect(operand.Value.(compiler.Indirect))
	case compiler.OperandKindRegister:
		return r.storeRegister(operand.Value.(compiler.Register))
	default:
		return func(_ float64, err error) error {
			if err != nil {
				return err
			}
			return fmt.Errorf("bad %s store operand in runtime", operand.Kind)
		}
	}
}

func (r *Runtime) Run(ctx context.Context) error {
	for _, varinit := range r.prog.VarInitFunctions() {
		err := r.RunFrom(ctx, varinit.Addr())
		if err != nil {
			return err
		}
	}

	for _, init := range r.prog.InitFunctions() {
		err := r.RunFrom(ctx, init.Addr())
		if err != nil {
			return err
		}
	}

	for _, update := range r.prog.UpdateFunctions() {
		err := r.RunFrom(ctx, update.Addr())
		if err != nil {
			return err
		}
	}

	return nil
}

func (r *Runtime) RunFrom(ctx context.Context, pc Addr) (err error) {
	for reg := range r.registers {
		r.registers[reg] = 0
	}

	r.setSP(Addr(r.prog.GlobalSize()))
	for range len(r.registers) - 2 {
		r.push(0)
	}

	r.push(0)

	r.setFP(r.sp() - 1)
	r.setPC(pc)

	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
			// case <-time.After(100 * time.Millisecond):
		default:
		}

		if r.sp() > 0x1000 {
			return fmt.Errorf("stack overflow")
		}

		code, err := r.fetch(r.pc())
		if err != nil {
			return err
		}

		if r.debug {
			log.Println("--------------")
			log.Printf("%s", code)
			log.Printf("fp: %v", r.fp())
			log.Printf("sp: %v", r.sp())
			log.Printf("pc: %v", r.pc())
			r.printRegisters()
			r.printStack()
		}

		r.setPC(r.pc() + 1)

		switch code := code.(type) {
		case nil:
			return fmt.Errorf("invalid nil bytecode: %w", err)
		case compiler.Nop:
		case compiler.Mov:
			for i := Size(0); i < code.Size; i++ {
				err := r.store(code.Dst.OffsetReference(i))(r.load(code.Src.OffsetReference(i))())
				if err != nil {
					return err
				}
			}
		case compiler.Cal:
			funcInfoAddr, err := r.load(code.Func)()
			if err != nil {
				return fmt.Errorf("could not resolve function info: %w", err)
			}

			funcType, err := r.loadAddr(Addr(funcInfoAddr))
			if err != nil {
				return fmt.Errorf("could not resolve function addr: %w", err)
			}

			switch int(funcType) {
			case RuntimeFuncTypeNil:
				return fmt.Errorf("tried to call nil function reference")
			case RuntimeFuncTypeExtern:
				externName, err := r.loadAddr(Addr(funcInfoAddr) + 3)
				if err != nil {
					return fmt.Errorf("could not resolve extern function name")
				}

				externNameStr, err := r.LoadString(Addr(externName))
				if err != nil {
					return fmt.Errorf("could not load extern function name string")
				}

				// TODO load string

				entry, ok := r.externFuncs[externNameStr]
				if !ok {
					return fmt.Errorf("undefined extern func %q %d", string(externNameStr), int(externName))
				}

				args, err := r.loadArgs(r.sp(), entry.ArgSize)
				if err != nil {
					return fmt.Errorf("failed to load %d args for extern %q", entry.ArgSize, code.Func)
				}
				ret := entry.Func(r, args)
				for i := range entry.ReturnSize {
					r.storeAddr(r.sp().Offset(-(entry.ArgSize + entry.ReturnSize)).Offset(-i), ret)
				}

				if r.debug {
					var argStrs []string
					for _, arg := range args {
						argStrs = append(argStrs, fmt.Sprintf("%v", arg))

					}
					log.Printf("extern call %s(%s) = %v", string(externNameStr), strings.Join(argStrs, ", "), ret)
				}

				r.setSP(r.sp() - compiler.Addr(entry.ArgSize+entry.ReturnSize))

				continue
			case RuntimeFuncTypeFunc:
				faddr, err := r.loadAddr(Addr(funcInfoAddr) + 3)
				if err != nil {
					return fmt.Errorf("could not resolve function addr")
				}

				frameSize := Addr(len(r.registers))

				for reg := range Register(len(r.registers)) {
					err := r.storeAddr(r.sp()+frameSize-Addr(reg)-1, r.registers[reg])
					if err != nil {
						return fmt.Errorf("failed to push %s", compiler.Register(reg))
					}
				}

				r.setSP(r.sp() + frameSize)
				r.setFP(r.sp() - 1)
				r.setPC(Addr(faddr))
				continue
			default:
				return fmt.Errorf("unhandled function type %d", int(funcType))
			}
		case compiler.Ret:
			fp := r.fp()

			for reg := range r.registers {
				r.registers[reg], err = r.loadAddr(fp.Offset(-compiler.Size(reg)))
				if err != nil {
					return fmt.Errorf("failed to push %s", Register(reg))
				}
			}

			r.setSP(r.sp().Offset(-code.Args))

			// exit completely
			if r.fp() == 0 {
				if r.debug {
					log.Println("--------------")
					log.Printf("EXIT")
					log.Printf("fp: %v", r.fp())
					log.Printf("sp: %v", r.sp())
					log.Printf("pc: %v", pc)
					r.printRegisters()
					r.printStack()
				}
				return nil
			}
		case compiler.Jmp:
			cond, err := r.load(code.Cond)()
			if err != nil {
				return err
			}

			if cond != 0 {
				val, err := r.load(code.Target)()
				if err != nil {
					return err
				}

				r.setPC(Addr(val))
			}
		case compiler.BinOp:
			err = r.store(code.Dst)(
				binaryOp(
					r,
					binaryOperatorFuncs[code.Op],
					r.load(code.Left),
					r.load(code.Right),
				),
			)
		case compiler.UnOp:
			err = r.store(code.Dst)(
				unaryOp(
					r,
					unaryOperatorFuncs[code.Op],
					r.load(code.Src),
				),
			)
		case compiler.Str:
			addr, err := r.allocStr(code.Str)
			if err != nil {
				return err
			}

			err = r.store(code.Dst)(float64(addr), nil)
			if err != nil {
				return err
			}
		case compiler.Alc:
			addr, err := r.alloc(code.Size)
			if err != nil {
				return err
			}

			err = r.store(code.Dst)(float64(addr), nil)
			if err != nil {
				return err
			}

		// case compiler.LAddr:
		// 	addr, err := r.load(code.Src)()
		// 	if err != nil {
		// 		return err
		// 	}
		//
		// 	err = r.store(code.Dst)(r.fp.Offset(AddrOffset(addr.(Int))), nil)
		// 	if err != nil {
		// 		return err
		// 	}
		default:
			return fmt.Errorf("unrecognized bytecode: %T %v", code, code)
		}

		if err != nil {
			return err
		}
	}
}

type loadFunc func() (float64, error)

type storeFunc func(float64, error) error

type binaryOperatorFunc func(*Runtime, float64, float64) (float64, error)

func binaryOp(r *Runtime, op binaryOperatorFunc, loadA, loadB loadFunc) (float64, error) {
	if op == nil {
		return 0, fmt.Errorf("invalid binary operation")
	}

	a, err := loadA()
	if err != nil {
		return 0, err
	}

	b, err := loadB()
	if err != nil {
		return 0, err
	}

	return op(r, a, b)
}

type unaryOperatorFunc func(*Runtime, float64) (float64, error)

func unaryOp(r *Runtime, op unaryOperatorFunc, loadA loadFunc) (float64, error) {
	if op == nil {
		return 0, fmt.Errorf("invalid unary operation")
	}

	a, err := loadA()
	if err != nil {
		return 0, err
	}

	return op(r, a)
}

type Float = compiler.Float
type Int = compiler.Int
type String = compiler.String
type Bool = compiler.Bool
type Addr = compiler.Addr
type Size = compiler.Size
type Register = compiler.Register

var binaryOperatorFuncs = map[compiler.Operation]binaryOperatorFunc{
	"I**I": mathBinOp(opExp[Int]),
	"F**F": mathBinOp(opExp[Float]),

	"I+I": mathBinOp(opAdd[Int]),
	"F+F": mathBinOp(opAdd[Float]),
	"S+S": opAddStr,

	"I-I": mathBinOp(opSub[Int]),
	"F-F": mathBinOp(opSub[Float]),

	"I*I": mathBinOp(opMul[Int]),
	"F*F": mathBinOp(opMul[Float]),

	"I/I": mathBinOp(opDiv[Int]),
	"F/F": mathBinOp(opDiv[Float]),

	"I%I": mathBinOp(opMod[Int]),

	"I<I": mathBinOp(opLT[Int]),
	"F<F": mathBinOp(opLT[Float]),

	"I<=I": mathBinOp(opLTE[Int]),
	"F<=F": mathBinOp(opLTE[Float]),

	"I>I": mathBinOp(opGT[Int]),
	"F>F": mathBinOp(opGT[Float]),

	"I>=I": mathBinOp(opGTE[Int]),
	"F>=F": mathBinOp(opGTE[Float]),

	"I==I": mathBinOp(opEQ[Int]),
	"F==F": mathBinOp(opEQ[Float]),
	"S==S": opEQStr,
	"B==B": mathBinOp(opEQ[Bool]),

	"I!=I": mathBinOp(opNE[Int]),
	"F!=F": mathBinOp(opNE[Float]),
	"S!=S": opNEStr,
	"B!=B": mathBinOp(opNE[Bool]),
}

func mathBinOp(f func(a, b float64) float64) binaryOperatorFunc {
	return binaryOperatorFunc(func(_ *Runtime, a, b float64) (float64, error) {
		return f(a, b), nil
	})
}

var unaryOperatorFuncs = map[compiler.Operation]unaryOperatorFunc{
	"-I": mathUnOp(opNeg[Int]),
	"-F": mathUnOp(opNeg[Float]),
}

func mathUnOp(f func(a float64) float64) unaryOperatorFunc {
	return unaryOperatorFunc(func(_ *Runtime, a float64) (float64, error) {
		return f(a), nil
	})
}

func opNeg[T Int | Float](a float64) float64 {
	return -float64(T(a))
}

func opExp[T Int | Float](a, b float64) float64 {
	return math.Pow(a, b)
}

func opAdd[T Int | Float](a, b float64) float64 {
	return float64(T(a) + T(b))
}

func opAddStr(r *Runtime, a, b float64) (float64, error) {
	aStr, err := r.LoadString(Addr(a))
	if err != nil {
		return 0, err
	}

	bStr, err := r.LoadString(Addr(b))
	if err != nil {
		return 0, err
	}

	cStr := aStr + bStr

	addr, err := r.allocStr(cStr)
	if err != nil {
		return 0, err
	}

	return float64(addr), nil
}

func opSub[T Int | Float](a, b float64) float64 {
	return float64(T(a) - T(b))
}

func opMul[T Int | Float](a, b float64) float64 {
	return float64(T(a) * T(b))
}

func opDiv[T Int | Float](a, b float64) float64 {
	return float64(T(a) / T(b))
}

func opMod[T Int](a, b float64) float64 {
	return float64(T(a) % T(b))
}

func opLT[T Int | Float](a, b float64) float64 {
	if a < b {
		return 1
	}
	return 0
}

func opLTE[T Int | Float](a, b float64) float64 {
	if a <= b {
		return 1
	}
	return 0
}

func opGT[T Int | Float](a, b float64) float64 {
	if a > b {
		return 1
	}
	return 0
}

func opGTE[T Int | Float](a, b float64) float64 {
	if a >= b {
		return 1
	}
	return 0
}

func opEQ[T Int | Float | Bool](a, b float64) float64 {
	if a == b {
		return 1
	}
	return 0
}

func opEQStr(r *Runtime, a, b float64) (float64, error) {
	aStr, err := r.LoadString(Addr(a))
	if err != nil {
		return 0, err
	}

	bStr, err := r.LoadString(Addr(b))
	if err != nil {
		return 0, err
	}

	if aStr == bStr {
		return 1, nil
	}

	return 0, nil
}

func opNE[T Int | String | Float | Bool](a, b float64) float64 {
	if a != b {
		return 1
	}
	return 0
}

func opNEStr(r *Runtime, a, b float64) (float64, error) {
	aStr, err := r.LoadString(Addr(a))
	if err != nil {
		return 0, err
	}

	bStr, err := r.LoadString(Addr(a))
	if err != nil {
		return 0, err
	}

	if aStr != bStr {
		return 1, nil
	}

	return 0, nil
}

func (r *Runtime) printRegisters() {
	regStrs := make([]string, 0, len(r.registers))
	for reg, regVal := range r.registers {
		regStrs = append(regStrs, fmt.Sprintf("%v=%v", Register(reg), regVal))
	}

	log.Printf("Registers: %s", strings.Join(regStrs, ", "))
}

func (r *Runtime) printStack() {
	var addr Addr

	for {
		val, err := r.loadAddr(addr)
		if err != nil {
			panic(err)
		}

		if addr == r.sp()+2 {
			return
		}

		log.Printf("%s - 0x%04x", addr, int(val))

		addr++
	}

}
