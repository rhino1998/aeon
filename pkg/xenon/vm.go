package xenon

import (
	"context"
	"fmt"
	"io"
	"log"
	"math"
	"os"
	"slices"
	"strings"

	"github.com/rhino1998/aeon/pkg/compiler"
	"github.com/rhino1998/aeon/pkg/compiler/air"
	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/operators"
	"github.com/rhino1998/aeon/pkg/compiler/types"
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

var ErrRuntimePanic = fmt.Errorf("panic")

func DefaultExternFuncs() RuntimeExternFuncs {
	return RuntimeExternFuncs{
		"__builtin_assert": {
			ArgSize:    2,
			ReturnSize: 0,
			Func: func(r *Runtime, s ...float64) error {
				str, err := r.LoadString(Addr(s[1]))
				if err != nil {
					return err
				}

				if s[0] == 0 {
					return fmt.Errorf("assertion failed: %s", string(str))
				}

				return nil
			},
		},
		"print": {
			ArgSize:    3,
			ReturnSize: 0,
			Func: func(r *Runtime, s ...float64) error {
				sliceData := Addr(s[0])
				sliceLen := int(s[1])

				elemSize := Size(2)

				for i := Size(0); i < Size(sliceLen); i++ {
					typ, err := r.loadAddr(sliceData.Offset(i * elemSize))
					if err != nil {
						return err
					}

					val, err := r.loadAddr(sliceData.Offset(i*elemSize + 1))
					if err != nil {
						return err
					}

					valStr, err := r.toString(typ, val)
					if err != nil {
						return err
					}

					fmt.Fprintf(r.stdout, "%s", valStr)
					if i < Size(sliceLen)-1 {
						fmt.Fprintf(r.stdout, " ")
					}
				}

				fmt.Fprintf(r.stdout, "\n")

				return nil
			},
		},
		"printf": {
			ArgSize:    4,
			ReturnSize: 0,
			Func: func(r *Runtime, s ...float64) error {

				fmtStr, err := r.LoadString(Addr(s[0]))
				if err != nil {
					return err
				}

				var _ = fmtStr

				sliceData := Addr(s[1])
				sliceLen := int(s[2])

				elemSize := Size(2)

				var args []any

				for i := Size(0); i < Size(sliceLen); i++ {
					typ, err := r.loadAddr(sliceData.Offset(i * elemSize))
					if err != nil {
						return err
					}

					val, err := r.loadAddr(sliceData.Offset(i*elemSize + 1))
					if err != nil {
						return err
					}

					valNative, err := r.toNative(typ, val)
					if err != nil {
						return err
					}

					args = append(args, valNative)
				}

				fmt.Fprintf(r.stdout, string(fmtStr), args...)
				fmt.Fprintf(r.stdout, "\n")

				return nil
			},
		},
		"itoa": {
			ArgSize:    1,
			ReturnSize: 1,
			Func: func(r *Runtime, s ...float64) error {
				addr, err := r.allocStr(String(fmt.Sprintf("%d", int(s[1]))))
				if err != nil {
					return err
				}
				s[0] = float64(addr)
				return nil
			},
		},
		"panic": {
			ArgSize: 2,
			Func: func(r *Runtime, s ...float64) error {
				valStr, err := r.toString(s[0], s[1])
				if err != nil {
					return err
				}
				return fmt.Errorf("%v", valStr)
			},
		},
	}
}

const PageSize = 65535 // annoyingly not a power of 2

type RuntimeExternFuncs map[String]RuntimeExternFuncEntry

type RuntimeExternFunc func(*Runtime, ...float64) error

type RuntimeTypeSlot struct {
	Size Size
	Type int
}

type RuntimeTypeAddr struct {
	Addr Addr
	Type int
}

type gcState struct {
	heapStart Addr
	heapEnd   Addr
	heapIndex Addr

	heapAllocs    []Addr
	heapVars      []Addr
	heapVarAllocs []Addr
	heapPtrs      []Addr
	heapUsedAlloc []Addr
	heapUsedSize  []Size
	heapUsedPtr   []Addr

	strHeapStart Addr
	strHeapEnd   Addr
	strHeapIndex Addr

	strVars []Addr
	strPtrs []Addr
	strUsed []Addr
}

func (g *gcState) addStrVar(v, ptr Addr) {
	if ptr >= g.strHeapStart && ptr < g.strHeapEnd {
		g.strVars = append(g.strVars, v)
		g.strPtrs = append(g.strPtrs, ptr)
	}

}

func (g *gcState) addHeapVar(v, ptr Addr) {
	if ptr >= g.heapStart && ptr < g.heapEnd {
		g.heapVars = append(g.heapVars, v)
		g.heapPtrs = append(g.heapPtrs, ptr)

		prevAlloc := g.heapStart
		for _, alloc := range g.heapAllocs {
			if ptr < alloc {
				g.heapVarAllocs = append(g.heapVarAllocs, prevAlloc)
				log.Println("adding", v, ptr, prevAlloc)
				break
			}

			prevAlloc = alloc
		}
	}
}

type Runtime struct {
	prog        *compiler.Program
	externFuncs RuntimeExternFuncs
	stdout      io.Writer

	debug bool

	codePages  [][PageSize]air.Instruction
	heapAllocs []Addr
	heapStart  Addr
	heapEnd    Addr
	heapIndex  Addr

	funcTrace []float64

	registers []float64

	memPages     [][PageSize]float64
	strHeapStart Addr
	strHeapEnd   Addr
	strHeapIndex Addr
	strPages     [][PageSize]String

	vtables   map[int]map[string]float64
	globalMap []RuntimeTypeSlot
	funcMap   map[int][]RuntimeTypeSlot
	typeMap   map[int][]RuntimeTypeSlot
}

func NewRuntime(prog *compiler.Program, externs RuntimeExternFuncs, memPages, strPages int, stdout io.Writer, debug bool) (*Runtime, error) {
	heapStart := Addr(memPages * PageSize / 2)

	r := &Runtime{
		prog:        prog,
		externFuncs: externs,
		stdout:      stdout,

		debug: debug,

		codePages: make([][PageSize]air.Instruction, (len(prog.Instructions())+PageSize-1)/PageSize),
		memPages:  make([][PageSize]float64, memPages),
		strPages:  make([][PageSize]String, strPages),
		registers: make([]float64, prog.Registers()),

		heapStart: heapStart,
		heapIndex: heapStart,
		heapEnd:   Addr(memPages * PageSize),

		vtables: make(map[int]map[string]float64),
		funcMap: make(map[int][]RuntimeTypeSlot),
		typeMap: make(map[int][]RuntimeTypeSlot),
	}

	if stdout == nil {
		r.stdout = os.Stdout
	}

	strMap := make(map[String]Addr)
	for i, str := range prog.Strings() {
		strMap[str] = Addr(Addr(i))
		page, pageAddr := r.splitAddr(Addr(i))
		r.strPages[page][pageAddr] = str
		r.strHeapIndex++

		// log.Printf("str %d: %s", i, str)
	}

	r.strHeapStart = r.strHeapIndex
	r.strHeapEnd = Addr(strPages * PageSize)

	for i, code := range prog.Instructions() {
		page, pageAddr := r.splitAddr(Addr(i))
		r.codePages[page][pageAddr] = code
	}

	typeIDFromName := make(map[types.Name]int)

	allFuncs := make(map[string]float64)

	for _, fun := range prog.AllFunctions() {
		allFuncs[fun.QualifiedName()] = float64(fun.InfoAddr())
	}

	for typeID, typ := range prog.Types() {
		r.vtables[typeID] = make(map[string]float64)
		if debug {
			log.Printf("type %v: %d", typ, typeID)
		}
		typSize, err := air.TypeSize(typ)
		if err != nil {
			return nil, err
		}

		r.vtables[typeID]["#size"] = float64(typSize)
		r.vtables[typeID]["#kind"] = float64(typ.Kind())
		r.vtables[typeID]["#name"] = float64(strMap[String(typ.GlobalName())])

		typeIDFromName[typ.GlobalName()] = typeID

		r.typeMap[typeID] = make([]RuntimeTypeSlot, 0)

		// TODO: method resolution
		switch typ := types.Dereference(typ).(type) {
		case *types.Derived:
			for _, method := range typ.Methods(false) {
				methodName := typ.MethodQualifiedName(false, method.Name)
				funAddr, ok := allFuncs[methodName]
				if !ok {
					return nil, fmt.Errorf("method %s not found", methodName)
				}
				r.vtables[typeID][method.Name] = funAddr
			}
		case *types.Pointer:
			switch typ := types.Dereference(typ.Pointee()).(type) {
			case *types.Derived:
				for _, method := range typ.Methods(true) {
					methodName := typ.MethodQualifiedName(true, method.Name)
					funAddr, ok := allFuncs[methodName]
					if !ok {
						return nil, fmt.Errorf("method %s not found", methodName)
					}
					r.vtables[typeID][method.Name] = funAddr
				}
			}
		}
	}

	for typeID, typ := range prog.Types() {
		switch typ := types.Resolve(typ).(type) {
		case *types.Pointer:
			r.vtables[typeID]["#pointee"] = float64(typeIDFromName[typ.Pointee().GlobalName()])
			// TODO: product types somehow
		case *types.Array:
			r.vtables[typeID]["#length"] = float64(typ.Length())
			r.vtables[typeID]["#elem"] = float64(typeIDFromName[typ.Elem().GlobalName()])
		case *types.Tuple:
			r.vtables[typeID]["#elems"] = float64(len(typ.Elems()))
			for i, elem := range typ.Elems() {
				r.vtables[typeID][fmt.Sprintf("#elem.%d", i)] = float64(typeIDFromName[elem.GlobalName()])
			}
		case *types.Slice:
			r.vtables[typeID]["#elem"] = float64(typeIDFromName[typ.Elem().GlobalName()])
		case *types.Variadic:
			r.vtables[typeID]["#elem"] = float64(typeIDFromName[typ.Elem().GlobalName()])
		case *types.Struct:
			r.vtables[typeID]["#fields"] = float64(len(typ.Fields()))
			for i, field := range typ.Fields() {
				r.vtables[typeID][fmt.Sprintf("#field.%d", i)] = float64(typeIDFromName[field.Type.GlobalName()])
				r.vtables[typeID][fmt.Sprintf("#field.%d.name", i)] = float64(strMap[String(field.Name)])
			}
		}
	}

	// r.printVTables()
	//
	globalLayout, err := prog.GlobalLayout()
	if err != nil {
		return nil, err
	}

	for _, slot := range globalLayout {
		typSize, err := air.TypeSize(slot.Type)
		if err != nil {
			return nil, err
		}
		r.globalMap = append(r.globalMap,
			RuntimeTypeSlot{Type: typeIDFromName[slot.Type.GlobalName()], Size: typSize})
	}

	for _, fun := range prog.AllFunctions() {
		stackLayout, err := fun.StackLayout()
		if err != nil {
			return nil, err
		}
		funLayout := make([]RuntimeTypeSlot, 0)

		for _, slot := range stackLayout {
			typSize, err := air.TypeSize(slot.Type)
			if err != nil {
				return nil, err
			}
			funLayout = append(funLayout,
				RuntimeTypeSlot{Type: typeIDFromName[slot.Type.GlobalName()], Size: typSize})
		}

		r.funcMap[int(fun.Addr())] = funLayout
	}

	return r, nil
}

func (r *Runtime) toNative(typ float64, value float64) (any, error) {
	kind, ok := r.vtables[int(typ)]["#kind"]
	if !ok {
		panic(fmt.Sprintf("runtime: could not resolve kind %d", int(typ)))
	}
	switch kinds.Kind(kind) {
	case kinds.Nil:
		return nil, nil
	case kinds.Bool:
		if value == 0 {
			return false, nil
		} else {
			return true, nil
		}
	case kinds.Int:
		return int(value), nil
	case kinds.Float:
		return value, nil
	case kinds.String:
		str, err := r.LoadString(Addr(value))
		if err != nil {
			return "", err
		}

		return string(str), nil
	default:
		return fmt.Sprintf("<unhandled type: %d>", int(typ)), nil
	}
}

func (r *Runtime) memmove(dst, src Addr, size Size) error {
	var err error
	tmp := make([]float64, size)
	for i := Size(0); i < size; i++ {
		tmp[i], err = r.loadAddr(src.Offset(i))
		if err != nil {
			return err
		}
	}

	for i := Size(0); i < size; i++ {
		err = r.storeAddr(dst.Offset(i), tmp[i])
		if err != nil {
			return err
		}
	}

	return nil
}

func (r *Runtime) memcpy(dst, src Addr, size Size) error {
	for i := Size(0); i < size; i++ {
		val, err := r.loadAddr(src.Offset(i))
		if err != nil {
			return err
		}

		err = r.storeAddr(dst.Offset(i), val)
		if err != nil {
			return err
		}
	}

	return nil
}

func (r *Runtime) memzero(dst Addr, size Size) error {
	for i := Size(0); i < size; i++ {
		err := r.storeAddr(dst.Offset(i), 0)
		if err != nil {
			return err
		}
	}

	return nil
}

func (r *Runtime) printMemUsage() {
	log.Printf("MEM: %d/%d heap, %d/%d str", r.heapIndex-r.heapStart, r.heapEnd-r.heapStart, r.strHeapIndex-r.strHeapStart, r.strHeapEnd-r.strHeapStart)
}

func (r *Runtime) gc(size Size) error {
	gc := gcState{
		heapAllocs: r.heapAllocs,
		heapStart:  r.heapStart,
		heapEnd:    r.heapEnd,
		heapIndex:  r.heapIndex,

		strHeapStart: r.strHeapStart,
		strHeapEnd:   r.strHeapEnd,
		strHeapIndex: r.strHeapIndex,
	}

	if float64(gc.heapIndex+Addr(size)-gc.heapStart)/float64(gc.heapEnd-gc.heapStart) < 1 &&
		float64(gc.strHeapIndex+1-gc.strHeapStart)/float64(gc.strHeapEnd-gc.strHeapStart) < 1 {
		return nil
	}

	err := gc.scanPointers(r)
	if err != nil {
		return err
	}

	err = gc.mark(r)
	if err != nil {
		return err
	}

	err = gc.compact(r)
	if err != nil {
		return err
	}

	r.strHeapIndex = gc.strHeapIndex

	r.heapAllocs = gc.heapAllocs
	r.heapIndex = gc.heapIndex

	return nil
}

func (gc *gcState) scanPointers(r *Runtime) error {
	var base Addr = 1
	for _, slot := range r.globalMap {
		var err error
		err = gc.scanTypePointers(r, slot.Type, base)
		if err != nil {
			return err
		}
		base += Addr(slot.Size)
	}

	for i := 1; i < len(r.funcTrace); i += 2 {
		funcType, err := r.loadAddr(Addr(r.funcTrace[i] + 0))
		if err != nil {
			return err
		}

		// funcName, err := r.loadAddr(Addr(r.funcTrace[i] + 1))
		// if err != nil {
		// 	return nil, nil, err
		// }
		//
		// fname, err := r.LoadString(Addr(funcName))
		// if err != nil {
		// 	return nil, nil, err
		// }

		switch funcType {
		case RuntimeFuncTypeNil, RuntimeFuncTypeExtern:
			continue
		case RuntimeFuncTypeFunc:
			funcAddr, err := r.loadAddr(Addr(r.funcTrace[i] + 3))
			if err != nil {
				return err
			}

			funcLayout := r.funcMap[int(funcAddr)]
			for _, slot := range funcLayout {
				err = gc.scanTypePointers(r, slot.Type, base)
				if err != nil {
					return err
				}
				base += Addr(slot.Size)
			}

		default:
			return fmt.Errorf("invalid function type %d", int(funcType))
		}
	}

	return nil
}

func (gc *gcState) scanTypePointers(r *Runtime, typeID int, addr Addr) error {
	var slots []RuntimeTypeAddr
	slots = append(slots, RuntimeTypeAddr{Type: typeID, Addr: addr})

	for len(slots) > 0 {
		slot := slots[len(slots)-1]
		slots = slots[:len(slots)-1]
		kind := kinds.Kind(r.vtables[slot.Type]["#kind"])

		// val, err := r.loadAddr(slot.Addr)
		// if err != nil {
		// 	return err
		// }

		// name, err := r.LoadString(Addr(r.vtables[slot.Type]["#name"]))
		// if err != nil {
		// 	return nil, nil, err
		// }
		//
		// log.Printf("%v %s = %v", slot.Addr, string(name), val)

		switch kind {
		case kinds.Nil, kinds.Void, kinds.Int, kinds.Bool, kinds.Float:
		case kinds.String:
			val, err := r.loadAddr(slot.Addr)
			if err != nil {
				return err
			}

			// str, err := r.LoadString(Addr(val))
			// if err != nil {
			// 	return err
			// }
			// log.Printf("%s = %s", slot.Addr, str)

			gc.addStrVar(slot.Addr, Addr(val))
		case kinds.Pointer:
			val, err := r.loadAddr(slot.Addr)
			if err != nil {
				return err
			}

			gc.addHeapVar(slot.Addr, Addr(val))

			slots = append(slots, RuntimeTypeAddr{Type: int(r.vtables[slot.Type]["#pointee"]), Addr: Addr(val)})
		case kinds.Array:
			length := int(r.vtables[slot.Type]["#length"])
			elem := int(r.vtables[slot.Type]["#elem"])
			elemSize := Size(r.vtables[elem]["#size"])

			for i := 0; i < length; i++ {
				slots = append(slots, RuntimeTypeAddr{Type: elem, Addr: slot.Addr.Offset(elemSize * Size(i))})
			}
		case kinds.Tuple:
			var totalOffset Size
			elems := int(r.vtables[slot.Type]["#elems"])
			for i := 0; i < elems; i++ {
				elem := int(r.vtables[slot.Type][fmt.Sprintf("#elem.%d", i)])
				elemSize := Size(r.vtables[elem]["#size"])

				slots = append(slots, RuntimeTypeAddr{Type: elem, Addr: slot.Addr.Offset(totalOffset)})

				totalOffset += elemSize
			}
		case kinds.Interface:
			typ, err := r.loadAddr(slot.Addr)
			if err != nil {
				return err
			}

			slots = append(slots, RuntimeTypeAddr{Type: int(typ), Addr: slot.Addr.Offset(1)})
		case kinds.Slice, kinds.Variadic:
			sliceData, err := r.loadAddr(slot.Addr)
			if err != nil {
				return err
			}

			sliceCap, err := r.loadAddr(slot.Addr + 2)
			if err != nil {
				return err
			}

			gc.addHeapVar(slot.Addr, Addr(sliceData))

			elem := int(r.vtables[slot.Type]["#elem"])
			elemSize := Size(r.vtables[elem]["#size"])

			for i := 0; i < int(sliceCap); i++ {
				slots = append(slots, RuntimeTypeAddr{Type: elem, Addr: Addr(sliceData).Offset(elemSize * Size(i))})
			}
		case kinds.Struct:
			var totalOffset Size
			elems := int(r.vtables[slot.Type]["#fields"])
			for i := 0; i < elems; i++ {
				elem := int(r.vtables[slot.Type][fmt.Sprintf("#field.%d", i)])
				elemSize := Size(r.vtables[elem]["#size"])

				slots = append(slots, RuntimeTypeAddr{Type: elem, Addr: slot.Addr.Offset(totalOffset)})

				totalOffset += elemSize
			}
		}
	}

	return nil
}

func (gc *gcState) mark(r *Runtime) error {
	prevHeapAddr := gc.heapStart
	for _, heapAllocAddr := range gc.heapAllocs {
		if slices.ContainsFunc(gc.heapPtrs, func(ptr Addr) bool {
			return ptr >= prevHeapAddr && ptr < heapAllocAddr
		}) {
			gc.heapUsedAlloc = append(gc.heapUsedAlloc, prevHeapAddr)
			gc.heapUsedSize = append(gc.heapUsedSize, Size(heapAllocAddr-prevHeapAddr))
		}

		prevHeapAddr = heapAllocAddr
	}

	for ptr := gc.strHeapStart; ptr <= gc.strHeapIndex; ptr++ {
		if slices.Contains(gc.strPtrs, ptr) {
			gc.strUsed = append(gc.strUsed, ptr)
		}
	}

	return nil
}

func (gc *gcState) compact(r *Runtime) error {
	gc.heapIndex = gc.heapStart
	newAllocs := make([]Addr, 0)
	for i := range gc.heapUsedAlloc {
		alloc := gc.heapUsedAlloc[i]
		size := gc.heapUsedSize[i]

		for index := range gc.heapVarAllocs {
			if gc.heapVarAllocs[index] != alloc {
				continue
			}

			varAddr := gc.heapVars[index]
			varVal, err := r.loadAddr(varAddr)
			if err != nil {
				return err
			}

			log.Println("moving", alloc, varAddr, varVal, gc.heapIndex, varVal-float64(alloc))

			err = r.storeAddr(varAddr, float64(gc.heapIndex)+(varVal-float64(alloc)))
			if err != nil {
				return err
			}
		}

		gc.heapIndex += Addr(size)
		newAllocs = append(newAllocs, gc.heapIndex)
	}

	gc.heapIndex = gc.heapStart
	for i := range gc.heapUsedAlloc {
		alloc := gc.heapUsedAlloc[i]
		size := gc.heapUsedSize[i]

		err := r.memmove(gc.heapIndex, alloc, size)
		if err != nil {
			return err
		}

		gc.heapIndex += Addr(size)
	}

	gc.heapAllocs = newAllocs

	gc.strHeapIndex = gc.strHeapStart
	for _, ptr := range gc.strUsed {
		str, err := r.LoadString(ptr)
		if err != nil {
			return err
		}
		err = r.storeStr(gc.strHeapIndex, str)
		if err != nil {
			return err
		}

		for _, v := range gc.strVars {
			val, err := r.loadAddr(v)
			if err != nil {
				return err
			}

			if Addr(val) == ptr {
				err = r.storeAddr(v, float64(gc.strHeapIndex))
				if err != nil {
					return err
				}
			}
		}

		gc.strHeapIndex++
	}

	return nil
}

func (r *Runtime) toString(typ float64, value float64) (string, error) {
	kind, ok := r.vtables[int(typ)]["#kind"]
	if !ok {
		panic(fmt.Sprintf("runtime: could not resolve kind for type %d", int(typ)))
	}

	switch kinds.Kind(kind) {
	case kinds.Nil:
		return "<nil>", nil
	case kinds.Bool:
		if value == 0 {
			return "false", nil
		} else {
			return "true", nil
		}
	case kinds.Int:
		return fmt.Sprintf("%d", int(value)), nil
	case kinds.Float:
		return fmt.Sprintf("%f", value), nil
	case kinds.String:
		str, err := r.LoadString(Addr(value))
		if err != nil {
			return "", err
		}

		return string(str), nil
	case kinds.Pointer:
		return fmt.Sprintf("%d", int(value)), nil
	default:
		return fmt.Sprintf("<unhandled type: %d>", int(typ)), nil
	}
}

func (r *Runtime) printVTables() {
	for typeID := range r.vtables {
		r.printVTable(typeID)
	}
}

func (r *Runtime) printVTable(typeID int) {
	name := r.vtables[typeID]["#name"]
	size := r.vtables[typeID]["#size"]
	kind := r.vtables[typeID]["#kind"]
	fmt.Fprintf(r.stdout, "Type %q %d (%d) %d\n", string(r.prog.Strings()[int(name)]), typeID, int(size), int(kind))

	switch kind := kinds.Kind(kind); kind {
	case kinds.Pointer:
		underlying := int(r.vtables[typeID]["#pointee"])
		fmt.Fprintf(r.stdout, "  Pointer to %d %q\n", underlying, string(r.prog.Strings()[int(r.vtables[underlying]["#name"])]))
	case kinds.Array:
		elem := int(r.vtables[typeID]["#elem"])
		length := int(r.vtables[typeID]["#length"])
		fmt.Fprintf(r.stdout, "  Array length %d of %d %q\n", length, elem, string(r.prog.Strings()[int(r.vtables[elem]["#name"])]))
	case kinds.Tuple:
		elems := int(r.vtables[typeID]["#elems"])
		for i := range elems {
			elem := int(r.vtables[typeID][fmt.Sprintf("#elem.%d", i)])
			fmt.Fprintf(r.stdout, "  Tuple element %d of %d %q\n", i, elem, string(r.prog.Strings()[int(r.vtables[elem]["#name"])]))
		}
	}

	for method, addr := range r.vtables[typeID] {
		if strings.HasPrefix(method, "#") {
			continue
		}
		fmt.Fprintf(r.stdout, "  %s -> %d\n", method, int(addr))
	}
}

func (r *Runtime) pc() Addr {
	return Addr(r.registers[air.RegisterPC])
}

func (r *Runtime) setPC(addr Addr) {
	r.registers[air.RegisterPC] = float64(addr)
}

func (r *Runtime) fp() Addr {
	return Addr(r.registers[air.RegisterFP])
}

func (r *Runtime) setFP(addr Addr) {
	r.registers[air.RegisterFP] = float64(addr)
}

func (r *Runtime) sp() Addr {
	return Addr(r.registers[air.RegisterSP])
}

func (r *Runtime) setSP(addr Addr) {
	r.registers[air.RegisterSP] = float64(addr)
}

func (r *Runtime) splitAddr(addr Addr) (uint64, uint64) {
	return uint64(addr / PageSize), uint64(addr % PageSize)
}

func (r *Runtime) CallExtern(name string, ret []float64, args ...float64) error {
	extern, ok := r.externFuncs[String(name)]
	if !ok {
		return fmt.Errorf("no such extern function %q", name)
	}

	ret = append(append([]float64{}, ret...), args...)

	return extern.Func(r, ret...)
}

func (r *Runtime) fetch(addr Addr) (air.Instruction, error) {
	page, pageAddr := r.splitAddr(addr)

	if page >= uint64(len(r.codePages)) {
		return nil, fmt.Errorf("invalid code page %v for addr %v", page, addr)
	}

	if pageAddr >= uint64(len(r.codePages[page])) {
		return nil, fmt.Errorf("invalid code page addr %v in page %v for addr %v", pageAddr, page, addr)
	}

	return r.codePages[page][pageAddr], nil
}

func (r *Runtime) loadAddr(addr Addr) (float64, error) {
	page, pageAddr := r.splitAddr(addr)

	if page >= uint64(len(r.memPages)) {
		return 0, fmt.Errorf("invalid page 0x%08x for addr %v", page, addr)
	}

	if pageAddr >= uint64(len(r.memPages[page])) {
		return 0, fmt.Errorf("invalid page addr 0x%08x in page 0x%08x for addr %v", pageAddr, page, addr)
	}

	return r.memPages[page][pageAddr], nil
}

func (r *Runtime) LoadString(addr Addr) (String, error) {
	page, pageAddr := r.splitAddr(addr)

	if page >= uint64(len(r.strPages)) {
		return "", fmt.Errorf("invalid str page 0x%08x for addr %v", page, addr)
	}

	if pageAddr >= uint64(len(r.strPages[page])) {
		return "", fmt.Errorf("invalid str page addr 0x%08x in page 0x%08x for addr %v", pageAddr, page, addr)
	}

	return r.strPages[page][pageAddr], nil
}

func (r *Runtime) loadArgs(sp Addr, size Size) ([]float64, error) {
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

func (r *Runtime) storeAddr(addr Addr, val float64) error {
	page, pageAddr := r.splitAddr(addr)

	if page >= uint64(len(r.memPages)) {
		return fmt.Errorf("invalid page 0x%08x for addr %v", page, addr)
	}

	if pageAddr >= uint64(len(r.memPages[page])) {
		return fmt.Errorf("invalid page addr 0x%08x in page 0x%08x for addr %v", pageAddr, page, addr)
	}

	r.memPages[page][pageAddr] = val

	return nil
}

func (r *Runtime) storeStr(addr Addr, val String) error {
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
	err := r.gc(0)
	if err != nil {
		return 0, err
	}
	r.strHeapIndex++

	page, pageAddr := r.splitAddr(r.strHeapIndex)

	if page >= uint64(len(r.strPages)) {
		return 0, fmt.Errorf("invalid str page 0x%08x for addr %v", page, r.strHeapIndex)
	}

	if pageAddr >= uint64(len(r.strPages[page])) {
		return 0, fmt.Errorf("invalid str page addr 0x%08x in page 0x%08x for addr %s", pageAddr, page, r.strHeapIndex)
	}

	r.strPages[page][pageAddr] = val

	return r.strHeapIndex, nil
}

func (r *Runtime) alloc(size Size) (Addr, error) {
	err := r.gc(size)
	if err != nil {
		return 0, err
	}

	addr := r.heapIndex
	r.heapIndex += Addr(size)
	r.heapAllocs = append(r.heapAllocs, r.heapIndex)
	if r.heapIndex > r.heapEnd {
		return 0, fmt.Errorf("out of memory")
	}

	err = r.memzero(addr, size)
	if err != nil {
		return 0, err
	}

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

func (r *Runtime) loadImmediate(imm air.Immediate) loadFunc {
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

func (r *Runtime) loadIndirect(indirect air.Indirect) loadFunc {
	return r.loadIndirectWithOffset(indirect.Ptr, 0)
}

func (r *Runtime) loadBinary(binop air.BinaryOperand) loadFunc {
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

func (r *Runtime) loadUnary(not air.UnaryOperand) loadFunc {
	return loadFunc(func() (float64, error) {
		a, err := r.load(not.A)()
		if err != nil {
			return 0, err
		}

		switch not.Op {
		case operators.Not:
			if a == 0 {
				return 1, nil
			}

			return 0, nil
		default:
			return 0, fmt.Errorf("unhandled unary operator %v", not.Op)
		}

	})
}

func (r *Runtime) loadIndirectWithOffset(indirect *air.Operand, offset Size) loadFunc {
	return loadFunc(func() (float64, error) {
		base, err := r.load(indirect)()
		if err != nil {
			return 0, err
		}

		return r.loadAddr(Addr(base).Offset(offset))
	})
}

func (r *Runtime) loadRegister(reg air.Register) loadFunc {
	return loadFunc(func() (float64, error) {
		return r.registers[reg], nil
	})
}

func (r *Runtime) loadVTable(lookup air.VTableLookup) loadFunc {
	return loadFunc(func() (float64, error) {
		typeID, err := r.load(lookup.Type)()
		if err != nil {
			return 0, fmt.Errorf("failed to get type id: %w", err)
		}

		typeNameAddr, ok := r.vtables[int(typeID)]["#name"]
		if !ok {
			return 0, fmt.Errorf("no such type %d", int(typeID))
		}

		typeName, err := r.LoadString(Addr(typeNameAddr))
		if err != nil {
			return 0, fmt.Errorf("failed to get type name: %w", err)
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
			r.printVTables()
			return 0, fmt.Errorf("no such vtable entry for %d %s %s", int(typeID), typeName, name)
		}

		return funAddr, nil
	})
}

func (r *Runtime) load(operand *air.Operand) loadFunc {
	switch operand.Kind {
	case air.OperandKindImmediate:
		return r.loadImmediate(operand.Value.(air.Immediate))
	case air.OperandKindIndirect:
		return r.loadIndirect(operand.Value.(air.Indirect))
	case air.OperandKindRegister:
		return r.loadRegister(operand.Value.(air.Register))
	case air.OperandKindBinary:
		return r.loadBinary(operand.Value.(air.BinaryOperand))
	case air.OperandKindUnary:
		return r.loadUnary(operand.Value.(air.UnaryOperand))
	case air.OperandKindVTableLookup:
		return r.loadVTable(operand.Value.(air.VTableLookup))
	default:
		return func() (float64, error) {
			return 0, fmt.Errorf("bad %s operand in runtime", operand.Kind)
		}
	}
}

func (r *Runtime) storeIndirect(indirect air.Indirect) storeFunc {
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

func (r *Runtime) storeRegister(reg air.Register) storeFunc {
	return storeFunc(func(val float64, err error) error {
		if err != nil {
			return err
		}

		r.registers[reg] = val
		return nil
	})
}

func (r *Runtime) store(operand *air.Operand) storeFunc {
	switch operand.Kind {
	case air.OperandKindIndirect:
		return r.storeIndirect(operand.Value.(air.Indirect))
	case air.OperandKindRegister:
		return r.storeRegister(operand.Value.(air.Register))
	default:
		return func(_ float64, err error) error {
			if err != nil {
				return err
			}
			return fmt.Errorf("bad %s store operand in runtime", operand.Kind)
		}
	}
}

func (r *Runtime) panic(err error) error {
	fmt.Fprintf(r.stdout, "panic: %v\n\n", err)
	for i := len(r.funcTrace) - 1; i >= 1; i -= 2 {
		funcAddr := Addr(r.funcTrace[i])

		funcNameAddr, err := r.loadAddr(funcAddr + 1)
		if err != nil {
			return err
		}

		funcName, err := r.LoadString(Addr(funcNameAddr))
		if err != nil {
			return err
		}

		funcFileAddr, err := r.loadAddr(funcAddr + 2)
		if err != nil {
			return err
		}

		funcFile, err := r.LoadString(Addr(funcFileAddr))
		if err != nil {
			return err
		}

		if i+1 >= len(r.funcTrace) {
			fmt.Fprintf(r.stdout, "%s()\n", string(funcName))
			fmt.Fprintf(r.stdout, "\t%s\n", string(funcFile))
		} else {
			line := int(r.funcTrace[i+1])
			fmt.Fprintf(r.stdout, "%s()\n", string(funcName))
			fmt.Fprintf(r.stdout, "\t%s:%d\n", string(funcFile), line)
		}
	}

	return err
}

func (r *Runtime) Run(ctx context.Context) error {
	for _, varinit := range r.prog.VarInitFunctions() {
		err := r.RunFunc(ctx, varinit.InfoAddr())
		if err != nil {
			return r.panic(err)
		}
	}

	for _, init := range r.prog.InitFunctions() {
		err := r.RunFunc(ctx, init.InfoAddr())
		if err != nil {
			return r.panic(err)
		}
	}

	for _, update := range r.prog.UpdateFunctions() {
		err := r.RunFunc(ctx, update.InfoAddr())
		if err != nil {
			return r.panic(err)
		}
	}

	r.printMemUsage()

	return nil
}

func (r *Runtime) RunFunc(ctx context.Context, funcAddr Addr) (err error) {
	pc, err := r.loadAddr(funcAddr + 3)
	if err != nil {
		return err
	}

	r.funcTrace = append(r.funcTrace, -1, float64(funcAddr))

	for reg := range r.registers {
		r.registers[reg] = 0
	}

	globalSize, err := r.prog.GlobalSize()
	if err != nil {
		return err
	}

	r.setSP(Addr(globalSize))
	for range len(r.registers) {
		r.push(0)
	}

	r.setFP(r.sp() - 1)
	r.setPC(Addr(pc))

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
			log.Printf("%v", code)
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
		case air.Nop:
		case air.Mov:
			tmp := make([]float64, code.Size)
			for i := Size(0); i < code.Size; i++ {
				tmp[i], err = r.load(code.Src.OffsetReference(i))()
				if err != nil {
					return err
				}
			}

			for i := Size(0); i < code.Size; i++ {
				err = r.store(code.Dst.OffsetReference(i))(tmp[i], nil)
				if err != nil {
					return err
				}
			}
		case air.Cal:
			funcInfoAddr, err := r.load(code.Func)()
			if err != nil {
				return fmt.Errorf("could not resolve function info: %w", err)
			}

			funcType, err := r.loadAddr(Addr(funcInfoAddr))
			if err != nil {
				return fmt.Errorf("could not resolve function addr: %w", err)
			}

			if r.debug {
				fNameAddr, err := r.loadAddr(Addr(funcInfoAddr) + 1)
				if err != nil {
					return err
				}

				fName, err := r.LoadString(Addr(fNameAddr))
				if err != nil {
					return err
				}

				log.Printf("call %s", string(fName))
			}

			r.funcTrace = append(r.funcTrace, float64(code.Line), funcInfoAddr)

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

				entry, ok := r.externFuncs[externNameStr]
				if !ok {
					return fmt.Errorf("undefined extern func %q %d", string(externNameStr), int(externName))
				}

				args, err := r.loadArgs(r.sp(), entry.ArgSize)
				if err != nil {
					return fmt.Errorf("failed to load %d args for extern %q", entry.ArgSize, code.Func)
				}

				retArgs := make([]float64, 0, entry.ReturnSize+entry.ArgSize)
				retArgs = append(retArgs, make([]float64, entry.ReturnSize)...)
				retArgs = append(retArgs, args...)

				err = entry.Func(r, retArgs...)
				if err != nil {
					return err
				}

				r.funcTrace = r.funcTrace[:len(r.funcTrace)-2]
				for i := range entry.ReturnSize {
					r.storeAddr(r.sp().Offset(-(entry.ArgSize + entry.ReturnSize)).Offset(-i), retArgs[i])
				}

				if r.debug {
					var argStrs []string
					for _, arg := range retArgs[entry.ReturnSize:] {
						argStrs = append(argStrs, fmt.Sprintf("%v", arg))
					}

					var retStrs []string
					for _, ret := range retArgs[:entry.ReturnSize] {
						retStrs = append(retStrs, fmt.Sprintf("%v", ret))
					}

					log.Printf("extern call %s(%s) = %v", string(externNameStr), strings.Join(argStrs, ", "), strings.Join(retStrs, ", "))
				}

				r.setSP(r.sp() - Addr(entry.ArgSize+entry.ReturnSize))

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
						return fmt.Errorf("failed to push %s", air.Register(reg))
					}
				}

				r.setSP(r.sp() + frameSize)
				r.setFP(r.sp() - 1)
				r.setPC(Addr(faddr))
				continue
			default:
				return fmt.Errorf("unhandled function type %d", int(funcType))
			}
		case air.Ret:
			fp := r.fp()

			for reg := range r.registers {
				r.registers[reg], err = r.loadAddr(fp.Offset(-air.Size(reg)))
				if err != nil {
					return fmt.Errorf("failed to push %s", Register(reg))
				}
			}

			r.setSP(r.sp().Offset(-code.Args))

			r.funcTrace = r.funcTrace[:len(r.funcTrace)-2]

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
		case air.Jmp:
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
		case air.BinOp:
			err = r.store(code.Dst)(
				binaryOp(
					r,
					binaryOperation(code.Kind, code.Op, code.Kind),
					r.load(code.Left),
					r.load(code.Right),
				),
			)
		case air.UnOp:
			err = r.store(code.Dst)(
				unaryOp(
					r,
					unaryOperation(code.Op, code.Kind),
					r.load(code.Src),
				),
			)
		case air.Alc:
			size, err := r.load(code.Size)()
			if err != nil {
				return err
			}

			addr, err := r.alloc(Size(size))
			if err != nil {
				return err
			}

			err = r.store(code.Dst)(float64(addr), nil)
			if err != nil {
				return err
			}
		case air.App:
			sliceData, err := r.load(code.Src.OffsetReference(0))()
			if err != nil {
				return err
			}
			sliceDataAddr := Addr(sliceData)

			sliceLen, err := r.load(code.Src.OffsetReference(1))()
			if err != nil {
				return err
			}

			sliceCap, err := r.load(code.Src.OffsetReference(2))()
			if err != nil {
				return err
			}

			if sliceCap < sliceLen {
				return fmt.Errorf("append: slice capacity less than length")
			}

			if sliceLen == sliceCap {
				if sliceCap == 0 {
					sliceCap = 1
				} else {
					sliceCap *= 2
				}

				newDataAddr, err := r.alloc(Size(sliceCap) * code.Size)
				if err != nil {
					return err
				}

				err = r.memcpy(newDataAddr, sliceDataAddr, Size(sliceLen)*code.Size)
				if err != nil {
					return err
				}

				sliceDataAddr = newDataAddr
			}

			if code.Size == 1 {
				val, err := r.load(code.Elem)()
				if err != nil {
					return err
				}
				r.storeAddr(sliceDataAddr.Offset(Size(sliceLen)*code.Size), val)
			} else {
				for i := Size(0); i < code.Size; i++ {
					val, err := r.load(code.Elem.OffsetReference(i))()
					if err != nil {
						return err
					}
					r.storeAddr(sliceDataAddr.Offset(Size(sliceLen)*code.Size).Offset(i), val)
				}
			}

			err = r.store(code.Dst.OffsetReference(0))(float64(sliceDataAddr), nil)
			if err != nil {
				return err
			}

			err = r.store(code.Dst.OffsetReference(1))(float64(sliceLen+1), nil)
			if err != nil {
				return err
			}

			err = r.store(code.Dst.OffsetReference(2))(float64(sliceCap), nil)
			if err != nil {
				return err
			}
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

func binaryOp(r *Runtime, opStr string, loadA, loadB loadFunc) (float64, error) {
	op, ok := binaryOperatorFuncs[opStr]
	if !ok {
		return 0, fmt.Errorf("invalid binary operation %q", opStr)
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

func unaryOp(r *Runtime, opStr string, loadA loadFunc) (float64, error) {
	op, ok := unaryOperatorFuncs[opStr]
	if !ok {
		return 0, fmt.Errorf("invalid unary operation %q", opStr)
	}

	a, err := loadA()
	if err != nil {
		return 0, err
	}

	return op(r, a)
}

type Float = air.Float
type Int = air.Int
type String = air.String
type Bool = air.Bool
type Addr = air.Addr
type Size = air.Size
type Register = air.Register

var binaryOperatorFuncs = map[string]binaryOperatorFunc{
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

	"B&&B": mathBinOp(opLAnd[Bool]),
	"B||B": mathBinOp(opLOr[Bool]),

	"I>=I": mathBinOp(opGTE[Int]),
	"F>=F": mathBinOp(opGTE[Float]),

	"I==I": mathBinOp(opEQ[Int]),
	"F==F": mathBinOp(opEQ[Float]),
	"S==S": opEQStr,
	"B==B": mathBinOp(opEQ[Bool]),
	"T==T": mathBinOp(opEQ[Int]),
	"P==P": mathBinOp(opEQ[Int]),

	"I!=I": mathBinOp(opNE[Int]),
	"F!=F": mathBinOp(opNE[Float]),
	"S!=S": opNEStr,
	"B!=B": mathBinOp(opNE[Bool]),
	"T!=T": mathBinOp(opNE[Int]),
	"P!=P": mathBinOp(opNE[Int]),
}

func binaryOperation(a kinds.Kind, op operators.Operator, b kinds.Kind) string {
	return fmt.Sprintf("%s%s%s", shortKind(a), op, shortKind(b))
}

func unaryOperation(op operators.Operator, a kinds.Kind) string {
	return fmt.Sprintf("%s%s", op, shortKind(a))
}

func shortKind(kind kinds.Kind) string {
	switch kind {
	case kinds.Int:
		return "I"
	case kinds.Float:
		return "F"
	case kinds.String:
		return "S"
	case kinds.Bool:
		return "B"
	case kinds.Pointer:
		return "P"
	case kinds.Type:
		return "T"
	default:
		return "?"
	}
}

func mathBinOp(f func(a, b float64) float64) binaryOperatorFunc {
	return binaryOperatorFunc(func(_ *Runtime, a, b float64) (float64, error) {
		return f(a, b), nil
	})
}

var unaryOperatorFuncs = map[string]unaryOperatorFunc{
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

func opLAnd[T Bool](a float64, b float64) float64 {
	if a != 0 && b != 0 {
		return 1
	}
	return 0
}

func opLOr[T Bool](a float64, b float64) float64 {
	if a != 0 || b != 0 {
		return 1
	}
	return 0
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
	for reg, regVal := range r.registers[3:] {
		regStrs = append(regStrs, fmt.Sprintf("%v=%v", Register(reg), Addr(regVal)))
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

		log.Printf("%v - 0x%04x", addr, int(val))

		addr++
	}
}
