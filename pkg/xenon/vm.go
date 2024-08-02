package xenon

import (
	"context"
	"fmt"
	"log"
	"math"
	"strings"
)

const PageSize = 65535 // annoyingly not a power of 2

type ExternFuncs map[string]ExternFunc

type ExternFunc func([]any) any

type Runtime struct {
	externs ExternFuncs

	codePages [][PageSize]Bytecode

	ret       any
	registers []any

	sp Addr
	fp Addr

	memPages [][PageSize]any
}

func NewRuntime(prog []Bytecode, externs ExternFuncs, memPages int, registers int) (*Runtime, error) {
	r := &Runtime{
		externs: externs,

		codePages: make([][PageSize]Bytecode, (len(prog)+PageSize-1)/PageSize),
		memPages:  make([][PageSize]any, memPages),
		registers: make([]any, registers),

		sp: 0,
		fp: 0,
	}

	for i, code := range prog {
		page, pageAddr := r.splitAddr(Addr(i))
		r.codePages[page][pageAddr] = code
	}

	r.registers[0] = int64(0)

	err := r.push(Addr(0))
	if err != nil {
		return nil, err
	}
	r.push(Addr(0))
	r.push(Addr(0))

	return r, nil
}

func (r *Runtime) splitAddr(addr Addr) (uint64, uint64) {
	return uint64(addr / PageSize), uint64(addr % PageSize)
}

func (r *Runtime) fetch(addr Addr) (Bytecode, error) {
	page, pageAddr := r.splitAddr(addr)

	if page >= uint64(len(r.codePages)) {
		return nil, fmt.Errorf("invalid code page 0x%08x for addr 0x%08x", page, addr)
	}

	if pageAddr >= uint64(len(r.codePages[page])) {
		return nil, fmt.Errorf("invalid code page addr 0x%08x in page 0x%08x for addr 0x%08x", pageAddr, page, addr)
	}

	return r.codePages[page][pageAddr], nil
}

func (r *Runtime) load(addr Addr) (any, error) {
	page, pageAddr := r.splitAddr(addr)

	if page >= uint64(len(r.memPages)) {
		return nil, fmt.Errorf("invalid page 0x%08x for addr 0x%08x", page, addr)
	}

	if pageAddr >= uint64(len(r.memPages[page])) {
		return nil, fmt.Errorf("invalid page addr 0x%08x in page 0x%08x for addr 0x%08x", pageAddr, page, addr)
	}

	return r.memPages[page][pageAddr], nil
}

func (r *Runtime) loadArgs(addr Addr, num int) ([]any, error) {
	var args []any
	for i := 0; i < num; i++ {
		arg, err := r.load(addr.Offset(AddrOffset(-i)))
		if err != nil {
			return nil, err
		}

		args = append(args, arg)
	}

	return args, nil
}

func (env *Runtime) store(addr Addr, val any) error {
	page := addr / 65536
	pageAddr := addr % 65536

	if page >= Addr(len(env.memPages)) {
		return fmt.Errorf("invalid page 0x%08x for addr 0x%08x", page, addr)
	}

	if pageAddr >= Addr(len(env.memPages[page])) {
		return fmt.Errorf("invalid page addr 0x%08x in page 0x%08x for addr 0x%08x", pageAddr, page, addr)
	}

	env.memPages[page][pageAddr] = val
	return nil
}

func (env *Runtime) push(val any) error {
	err := env.store(env.sp, val)
	env.sp++
	return err
}

func (r *Runtime) pop() (any, error) {
	val, err := r.load(r.sp)
	r.zeroStack()
	r.sp--
	return val, err
}

func (r *Runtime) zeroStack() {
	addr := r.sp
	for {
		val, err := r.load(addr)
		if err != nil {
			return
		}

		if val == nil {
			return
		}

		_ = r.store(addr, nil)
	}
}

func (r *Runtime) Run(ctx context.Context, pc Addr) (err error) {
	loadM := func(addr Addr) loadFunc {
		return loadFunc(func() (any, error) {
			return r.load(addr)
		})
	}

	loadO := func(offset AddrOffset) loadFunc {
		return loadFunc(func() (any, error) {
			return r.load(r.fp.Offset(offset))
		})
	}

	loadR := func(reg Register) loadFunc {
		return loadFunc(func() (any, error) {
			return r.registers[reg], nil
		})
	}

	loadI := func(imm Immediate) loadFunc {
		return loadFunc(func() (any, error) {
			return imm, nil
		})
	}

	load := func(operand Operand) loadFunc {
		switch operand.Source {
		case ValueSourceImmediate:
			return loadI(operand.Value.(Immediate))
		case ValueSourceRegister:
			return loadR(operand.Value.(Register))
		case ValueSourceMemory:
			return loadM(operand.Value.(Addr))
		case ValueSourceLocal:
			return loadO(operand.Value.(AddrOffset))
		default:
			return func() (any, error) {
				return nil, fmt.Errorf("invalid source type %v", operand.Source)
			}
		}
	}

	storeM := func(addr Addr) func(any, error) error {
		return func(val any, err error) error {
			if err != nil {
				return err
			}

			return r.store(addr, val)
		}
	}

	storeO := func(offset AddrOffset) func(any, error) error {
		return func(val any, err error) error {
			if err != nil {
				return err
			}

			return r.store(r.fp.Offset(offset), val)
		}
	}

	storeR := func(reg Register) func(any, error) error {
		return func(val any, err error) error {
			if err != nil {
				return err
			}

			r.registers[reg] = val
			return nil
		}
	}

	store := func(operand Operand) storeFunc {
		switch operand.Source {
		case ValueSourceRegister:
			return storeR(operand.Value.(Register))
		case ValueSourceMemory:
			return storeM(operand.Value.(Addr))
		case ValueSourceLocal:
			return storeO(operand.Value.(AddrOffset))
		default:
			return func(any, error) error {
				return fmt.Errorf("invalid source type %v", operand.Source)
			}
		}
	}

	// Fake frame
	r.sp = 3
	r.fp = 2

	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		// case <-time.After(100 * time.Millisecond):
		default:
		}

		code, err := r.fetch(pc)
		if err != nil {
			return err
		}

		log.Println("--------------")
		log.Printf("%s", code)
		log.Printf("fp: %v", r.fp)
		r.printRegisters()
		r.printStack()

		pc++

		switch code := code.(type) {
		case nil:
			return fmt.Errorf("invalid nil bytecode: %w", err)
		case Nop:
		case Mov:
			err = store(code.Dst)(load(code.Src)())
		case Store:
			addr, err := load(code.Dst)()
			if err != nil {
				return err
			}

			err = storeM(addr.(Addr))(load(code.Src)())
			if err != nil {
				return err
			}
		case Load:
			addr, err := load(code.Src)()
			if err != nil {
				return err
			}
			err = store(code.Dst)(loadM(addr.(Addr))())
			if err != nil {
				return err
			}
		case Push:
			val, err := load(code.Src)()
			if err != nil {
				return err
			}

			err = r.push(val)
			if err != nil {
				return err
			}
		case CallAddr:
			err := r.push(r.sp)
			if err != nil {
				return fmt.Errorf("failed to push sp: %w", err)
			}

			err = r.push(r.fp)
			if err != nil {
				return fmt.Errorf("failed to push sp: %w", err)
			}

			err = r.push(pc)
			if err != nil {
				return fmt.Errorf("failed to push next pc: %w", err)
			}

			r.fp = r.sp
			pc = code.Func
		case CallExtern:
			args, err := r.loadArgs(r.sp, code.Args)
			if err != nil {
				return fmt.Errorf("failed to load %d args for extern %q", code.Args, code.Extern)
			}
			r.ret = r.externs[code.Extern](args)
		case Return:
			r.sp, err = loadType[Addr](r, r.fp-2)
			if err != nil {
				return fmt.Errorf("failed to load sp from fp-2: %w", err)
			}

			r.fp, err = loadType[Addr](r, r.fp-1)
			if err != nil {
				return fmt.Errorf("failed to load fp from fp-1: %w", err)
			}

			pc, err = loadType[Addr](r, r.fp)
			if err != nil {
				return fmt.Errorf("failed to load pc from fp: %w", err)
			}

			r.zeroStack()

			// exit completely
			if pc == 0 {
				log.Printf("%v", r.registers[0])
				return nil
			}
		case Jmp:
			val, err := load(code.Dst)()
			if err != nil {
				return err
			}

			pc = Addr(val.(Int))
		case JmpR:
			val, err := load(code.Dst)()
			if err != nil {
				return err
			}

			pc = pc.Offset(AddrOffset(val.(Int)))
		case JmpRC:
			cond, err := load(code.Src)()
			if err != nil {
				return err
			}

			if cond.(Bool) != Bool(code.Invert) {
				val, err := load(code.Dst)()
				if err != nil {
					return err
				}

				pc = pc.Offset(AddrOffset(val.(Int)))
			}
		case Make:
			switch code.Kind {
			case "T":
				size, err := load(code.Size)()
				if err != nil {
					return err
				}

				err = store(code.Dst)(make(Tuple, size.(Int)), nil)
				if err != nil {
					return err
				}
			default:
				return fmt.Errorf("unimplemented kind %q", code.Kind)
			}
		case SetIndex:
			switch code.Kind {
			case "T":
				tuple, err := load(code.Base)()
				if err != nil {
					return err
				}

				index, err := load(code.Index)()
				if err != nil {
					return err
				}

				src, err := load(code.Src)()
				if err != nil {
					return err
				}

				tuple.(Tuple)[index.(Int)] = src

				err = store(code.Base)(tuple, nil)
				if err != nil {
					return err
				}
			default:
				return fmt.Errorf("unimplemented kind %q", code.Kind)
			}
		case BinOp:
			err = store(code.Dst)(
				binaryOp(
					binaryOperatorFuncs[code.Op],
					load(code.Left),
					load(code.Right),
				),
			)
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

var binaryOperatorFuncs = map[Operator]binaryOperatorFunc{
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
	return a.(T) - b.(T)
}

func opMul[T Int | Float](a, b any) any {
	return a.(T) * b.(T)
}

func opDiv[T Int | Float](a, b any) any {
	return a.(T) / b.(T)
}

func opMod[T Int](a, b any) any {
	return a.(T) % b.(T)
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
	return a.(T) == b.(T)
}

func opNE[T Int | String | Float | Bool](a, b any) any {
	return a.(T) != b.(T)
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
		val, err := r.load(addr)
		if err != nil {
			return
		}

		if val == nil {
			return
		}

		log.Printf("%s - %s", addr, val)

		addr++
	}

}

func loadType[T any](env *Runtime, addr Addr) (T, error) {
	raw, err := env.load(addr)
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
