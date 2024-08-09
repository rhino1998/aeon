package xenon

import (
	"context"
	"fmt"
	"log"
	"math"
	"strings"

	"github.com/rhino1998/aeon/pkg/compiler"
)

type RuntimeExternFuncEntry struct {
	Args int
	Func RuntimeExternFunc
}

func DefaultExternFuncs() RuntimeExternFuncs {
	return RuntimeExternFuncs{
		"print": {
			Args: 1,
			Func: func(s []any) any {
				log.Println(s[0])
				return nil
			},
		},
		"panic": {
			Args: 1,
			Func: func(s []any) any {
				panic(s[0])
			},
		},
		"add": {
			Args: 2,
			Func: func(s []any) any {
				return opAdd[Int](s[0], s[1])
			},
		},
	}
}

const PageSize = 65535 // annoyingly not a power of 2

type RuntimeExternFuncs map[string]RuntimeExternFuncEntry

type RuntimeExternFunc func([]any) any

type Runtime struct {
	prog        *compiler.Program
	externFuncs RuntimeExternFuncs

	debug bool

	codePages [][PageSize]compiler.Bytecode

	registers []any

	memPages [][PageSize]any
}

func NewRuntime(prog *compiler.Program, externs RuntimeExternFuncs, memPages int, registers int, debug bool) (*Runtime, error) {
	r := &Runtime{
		prog:        prog,
		externFuncs: externs,

		debug: debug,

		codePages: make([][PageSize]compiler.Bytecode, (len(prog.Bytecode())+PageSize-1)/PageSize),
		memPages:  make([][PageSize]any, memPages),
		registers: make([]any, registers),
	}

	for i, code := range prog.Bytecode() {
		page, pageAddr := r.splitAddr(compiler.Addr(i))
		r.codePages[page][pageAddr] = code
	}

	return r, nil
}

func (r *Runtime) fp() compiler.Addr {
	return Addr(r.registers[compiler.RegisterFP].(compiler.Int))
}

func (r *Runtime) setFP(addr compiler.Addr) {
	r.registers[compiler.RegisterFP] = Int(addr)
}

func (r *Runtime) sp() compiler.Addr {
	return Addr(r.registers[compiler.RegisterSP].(compiler.Int))
}

func (r *Runtime) setSP(addr compiler.Addr) {
	r.registers[compiler.RegisterSP] = Int(addr)
}

func (r *Runtime) splitAddr(addr compiler.Addr) (uint64, uint64) {
	return uint64(addr / PageSize), uint64(addr % PageSize)
}

func (r *Runtime) fetch(addr compiler.Addr) (compiler.Bytecode, error) {
	page, pageAddr := r.splitAddr(addr)

	if page >= uint64(len(r.codePages)) {
		return nil, fmt.Errorf("invalid code page 0x%08x for addr %s", page, addr)
	}

	if pageAddr >= uint64(len(r.codePages[page])) {
		return nil, fmt.Errorf("invalid code page addr 0x%08x in page 0x%08x for addr %s", pageAddr, page, addr)
	}

	return r.codePages[page][pageAddr], nil
}

func (r *Runtime) loadAddr(addr compiler.Addr) (any, error) {
	page, pageAddr := r.splitAddr(addr)

	if page >= uint64(len(r.memPages)) {
		return nil, fmt.Errorf("invalid page 0x%08x for addr 0x%08x", page, addr)
	}

	if pageAddr >= uint64(len(r.memPages[page])) {
		return nil, fmt.Errorf("invalid page addr 0x%08x in page 0x%08x for addr %s", pageAddr, page, addr)
	}

	return r.memPages[page][pageAddr], nil
}

func (r *Runtime) loadArgs(sp compiler.Addr, num int) ([]any, error) {
	var args []any
	for i := 0; i < num; i++ {
		arg, err := r.loadAddr(sp.Offset(compiler.AddrOffset(-(i + 1))))
		if err != nil {
			return nil, err
		}

		args = append(args, arg)
	}

	return args, nil
}

func (env *Runtime) storeAddr(addr compiler.Addr, val any) error {
	page := addr / 65536
	pageAddr := addr % 65536

	if page >= compiler.Addr(len(env.memPages)) {
		return fmt.Errorf("invalid page 0x%08x for addr 0x%08x", page, addr)
	}

	if pageAddr >= compiler.Addr(len(env.memPages[page])) {
		return fmt.Errorf("invalid page addr 0x%08x in page 0x%08x for addr 0x%08x", pageAddr, page, addr)
	}

	env.memPages[page][pageAddr] = val
	return nil
}

func (env *Runtime) push(val any) error {
	err := env.storeAddr(env.sp(), val)
	env.setSP(env.sp() + 1)
	return err
}

func (r *Runtime) pop() (any, error) {
	val, err := r.loadAddr(r.sp())
	r.zeroStack()
	r.setSP(r.sp() - 1)
	return val, err
}

func (r *Runtime) zeroStack() {
	addr := r.sp()
	for {
		val, err := r.loadAddr(addr)
		if err != nil {
			return
		}

		if val == nil {
			return
		}

		_ = r.storeAddr(addr, nil)
	}
}

func (r *Runtime) loadImmediate(imm compiler.Immediate) loadFunc {
	return loadFunc(func() (any, error) {
		return imm, nil
	})
}

func (r *Runtime) loadIndirect(indirect compiler.Indirect) loadFunc {
	return r.loadIndirectWithOffset(indirect, 0)
}

func (r *Runtime) loadOffset(offset compiler.Offset) loadFunc {
	return loadFunc(func() (any, error) {
		a, err := r.load(offset.A)()
		if err != nil {
			return nil, err
		}

		b, err := r.load(offset.B)()
		if err != nil {
			return nil, err
		}

		return a.(Int) + b.(Int), nil
	})
}

func (r *Runtime) loadIndirectWithOffset(indirect compiler.Indirect, offset AddrOffset) loadFunc {
	return loadFunc(func() (any, error) {
		base, err := r.load(indirect.Ptr)()
		if err != nil {
			return nil, err
		}

		return r.loadAddr(Addr(base.(compiler.Int)).Offset(offset))
	})
}

func (r *Runtime) loadRegister(reg compiler.Register) loadFunc {
	return loadFunc(func() (any, error) {
		return r.registers[reg], nil
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
	case compiler.OperandKindOffset:
		return r.loadOffset(operand.Value.(compiler.Offset))
	default:
		return nil
	}
}

func (r *Runtime) storeIndirect(indirect compiler.Indirect) storeFunc {
	return storeFunc(func(val any, err error) error {
		if err != nil {
			return err
		}

		ptr, err := r.load(indirect.Ptr)()
		if err != nil {
			return err
		}

		return r.storeAddr(Addr(ptr.(compiler.Int)), val)
	})
}

func (r *Runtime) storeRegister(reg compiler.Register) storeFunc {
	return storeFunc(func(val any, err error) error {
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
		return nil
	}
}

func (r *Runtime) Run(ctx context.Context, entryPoint string) (err error) {
	for reg := range r.registers {
		r.registers[reg] = Int(0)
	}

	r.setSP(Addr(r.prog.GlobalSize()))
	for range len(r.registers) - 2 {
		r.push(Int(0))
	}

	r.push(compiler.Int(0))

	r.setFP(r.sp() - 1)

	parts := strings.Split(entryPoint, ".")
	pkg, ok := r.prog.Package(parts[0])
	if !ok {
		return fmt.Errorf("failed to get entry point package: %s", parts[0])
	}
	f, ok := pkg.Function(parts[1])
	if !ok {
		return fmt.Errorf("failed to get entry point: %s", entryPoint)
	}

	pc := f.Addr()

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

		code, err := r.fetch(pc)
		if err != nil {
			return err
		}

		if r.debug {
			log.Println("--------------")
			log.Printf("%s", code)
			log.Printf("fp: %v", r.fp())
			log.Printf("sp: %v", r.sp())
			log.Printf("pc: %v", pc)
			r.printRegisters()
			r.printStack()
		}

		pc++

		switch code := code.(type) {
		case nil:
			return fmt.Errorf("invalid nil bytecode: %w", err)
		case compiler.Nop:
		case compiler.Mov:
			for i := AddrOffset(0); i < code.Size; i++ {
				err := r.store(code.Dst.OffsetReference(i))(r.load(code.Src.OffsetReference(i))())
				if err != nil {
					return err
				}
			}
		case compiler.Call:
			funcType, err := r.loadIndirectWithOffset(code.Func.Value.(compiler.Indirect), 0)()
			if err != nil {
				return fmt.Errorf("could not resolve function addr")
			}

			r.setSP(r.sp().Offset(code.Args))

			switch funcType.(Int) {
			case 0:
				externName, err := r.loadIndirectWithOffset(code.Func.Value.(compiler.Indirect), 3)()
				if err != nil {
					return fmt.Errorf("could not resolve extern function name")
				}

				entry, ok := r.externFuncs[string(externName.(String))]
				if !ok {
					return fmt.Errorf("undefined extern func %q", string(externName.(String)))
				}

				args, err := r.loadArgs(r.sp(), entry.Args)
				if err != nil {
					return fmt.Errorf("failed to load %d args for extern %q", entry.Args, code.Func)
				}
				ret := entry.Func(args)
				if ret != nil {
					r.storeAddr(r.sp()-Addr(entry.Args+1), ret)
				}

				if r.debug {
					var argStrs []string
					for _, arg := range args {
						argStrs = append(argStrs, fmt.Sprintf("%v", arg))

					}
					log.Printf("extern call %s = %s(%s)", ret, string(externName.(String)), strings.Join(argStrs, ", "))
				}

				continue
			case 1:
				faddr, err := r.loadIndirectWithOffset(code.Func.Value.(compiler.Indirect), 3)()
				if err != nil {
					return fmt.Errorf("could not resolve function addr")
				}

				frameSize := Addr(len(r.registers))

				for i := len(r.registers) - 1; i >= 1; i-- {
					reg := Register(i)
					err := r.storeAddr(r.sp()+frameSize-Addr(i)-1, r.registers[reg])
					if err != nil {
						return fmt.Errorf("failed to push %s", compiler.Register(reg))
					}
				}

				err = r.storeAddr(r.sp()+frameSize-1, Int(pc))
				if err != nil {
					return fmt.Errorf("failed to push next pc: %w", err)
				}

				r.setSP(r.sp() + frameSize)
				r.setFP(r.sp() - 1)
				pc = Addr(faddr.(Int))
				continue
			}

			return fmt.Errorf("unhandled function type")
		case compiler.Return:
			fp := r.fp()

			pcInt, err := loadType[Int](r, fp)
			if err != nil {
				return fmt.Errorf("failed to load pc from fp: %w", err)
			}

			pc = Addr(pcInt)

			for reg := range r.registers[1:] {
				r.registers[reg+1], err = r.loadAddr(fp.Offset(-1).Offset(-compiler.AddrOffset(reg)))
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
			val, err := r.load(code.Dst)()
			if err != nil {
				return err
			}

			pc = Addr(val.(Int))
		case compiler.JmpR:
			val, err := r.load(code.Dst)()
			if err != nil {
				return err
			}

			pc = pc.Offset(AddrOffset(val.(Int)))
		case compiler.JmpRC:
			cond, err := r.load(code.Src)()
			if err != nil {
				return err
			}

			if cond.(Bool) != Bool(code.Invert) {
				val, err := r.load(code.Dst)()
				if err != nil {
					return err
				}

				pc = pc.Offset(AddrOffset(val.(Int)))
			}
		case compiler.BinOp:
			err = r.store(code.Dst)(
				binaryOp(
					binaryOperatorFuncs[code.Op],
					r.load(code.Left),
					r.load(code.Right),
				),
			)
		case compiler.UnOp:
			switch code.Op {
			default:
				return fmt.Errorf("unrecognized unary operator %q", code.Op)
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

type loadFunc func() (any, error)

type storeFunc func(any, error) error

type binaryOperatorFunc func(any, any) any

func binaryOp(op binaryOperatorFunc, loadA, loadB loadFunc) (any, error) {
	if op == nil {
		return nil, fmt.Errorf("invalid binary operation")
	}

	a, err := loadA()
	if err != nil {
		return nil, err
	}

	b, err := loadB()
	if err != nil {
		return nil, err
	}

	return op(a, b), nil
}

type Float = compiler.Float
type Int = compiler.Int
type String = compiler.String
type Bool = compiler.Bool
type Addr = compiler.Addr
type AddrOffset = compiler.AddrOffset
type Register = compiler.Register

var binaryOperatorFuncs = map[compiler.Operation]binaryOperatorFunc{
	"I**I": opExp[Int],
	"F**F": opExp[Float],

	"I+I": opAdd[Int],
	"F+F": opAdd[Float],
	"S+S": opAdd[String],

	"I-I": opSub[Int],
	"F-F": opSub[Float],

	"I*I": opMul[Int],
	"F*F": opMul[Float],

	"I/I": opDiv[Int],
	"F/F": opDiv[Float],

	"I%I": opMod[Int],

	"I<I": opLT[Int],
	"F<F": opLT[Float],

	"I<=I": opLTE[Int],
	"F<=F": opLTE[Float],

	"I>I": opGT[Int],
	"F>F": opGT[Float],

	"I>=I": opGTE[Int],
	"F>=F": opGTE[Float],

	"I==I": opEQ[Int],
	"F==F": opEQ[Float],
	"S==S": opEQ[String],
	"B==B": opEQ[Bool],

	"I!=I": opNE[Int],
	"F!=F": opNE[Float],
	"S!=S": opNE[String],
	"B!=B": opNE[Bool],
}

func opExp[T Int | Float](a, b any) any {
	return T(math.Pow(float64(a.(T)), float64(b.(T))))
}

func opAdd[T Int | String | Float](a, b any) any {
	return a.(T) + b.(T)
}

func opSub[T Int | Float](a, b any) any {
	return T(a.(T) - b.(T))
}

func opMul[T Int | Float](a, b any) any {
	return T(a.(T) * b.(T))
}

func opDiv[T Int | Float](a, b any) any {
	return T(a.(T) / b.(T))
}

func opMod[T Int](a, b any) any {
	return T(a.(T) % b.(T))
}

func opLT[T Int | Float](a, b any) any {
	return Bool(a.(T) < b.(T))
}

func opLTE[T Int | Float](a, b any) any {
	return Bool(a.(T) <= b.(T))
}

func opGT[T Int | Float](a, b any) any {
	return Bool(a.(T) > b.(T))
}

func opGTE[T Int | Float](a, b any) any {
	return Bool(a.(T) >= b.(T))
}

func opEQ[T Int | String | Float | Bool](a, b any) any {
	return Bool(a.(T) == b.(T))
}

func opNE[T Int | String | Float | Bool](a, b any) any {
	return Bool(a.(T) != b.(T))
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

		if addr == r.sp() {
			return
		}

		log.Printf("%s - %s", addr, val)

		addr++
	}

}

func loadType[T any](env *Runtime, addr Addr) (T, error) {
	raw, err := env.loadAddr(addr)
	if err != nil {
		var ret T
		return ret, err
	}

	t, ok := raw.(T)
	if !ok {
		var ret T
		return ret, fmt.Errorf("expected type %T, got %T", ret, raw)
	}

	return t, nil
}

func pop[T any](env *Runtime) (T, error) {
	raw, err := env.pop()
	if err != nil {
		var ret T
		return ret, err
	}

	t, ok := raw.(T)
	if !ok {
		var ret T
		return ret, fmt.Errorf("expected type %T, got %T", ret, raw)
	}

	return t, nil
}
