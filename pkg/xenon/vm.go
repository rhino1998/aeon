package xenon

import (
	"context"
	"fmt"
	"io"
	"log"
	"os"
	"slices"
	"strings"

	"github.com/rhino1998/aeon/pkg/compiler"
	"github.com/rhino1998/aeon/pkg/compiler/abc"
	"github.com/rhino1998/aeon/pkg/compiler/air"
	"github.com/rhino1998/aeon/pkg/compiler/kinds"
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
					typ, err := r.loadMem(sliceData.Offset(i * elemSize))
					if err != nil {
						return err
					}

					val, err := r.loadMem(sliceData.Offset(i*elemSize + 1))
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
					typ, err := r.loadMem(sliceData.Offset(i * elemSize))
					if err != nil {
						return err
					}

					val, err := r.loadMem(sliceData.Offset(i*elemSize + 1))
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

	codePages  [][PageSize]float64
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
}

func NewRuntime(prog *compiler.Program, externs RuntimeExternFuncs, memPages, strPages int, stdout io.Writer, debug bool) (*Runtime, error) {
	heapStart := Addr(memPages * PageSize / 2)

	// TODO: do this elsewhere
	bytecode := abc.Compile(prog.Instructions())

	r := &Runtime{
		prog:        prog,
		externFuncs: externs,
		stdout:      stdout,

		debug: debug,

		codePages: make([][PageSize]float64, (len(bytecode)+PageSize-1)/PageSize),
		memPages:  make([][PageSize]float64, memPages),
		strPages:  make([][PageSize]String, strPages),
		registers: make([]float64, prog.Registers()),

		heapStart: heapStart,
		heapIndex: heapStart,
		heapEnd:   Addr(memPages * PageSize),

		vtables: make(map[int]map[string]float64),
		funcMap: make(map[int][]RuntimeTypeSlot),
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

	var codeAddr Addr
	for _, instruction := range bytecode {
		for _, uop := range instruction {
			page, pageAddr := r.splitAddr(codeAddr)
			r.codePages[page][pageAddr] = uop
			codeAddr++
		}
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

		log.Println(typ, types.HasPointer(typ))
		if types.HasPointer(typ) {
			r.vtables[typeID]["#has-pointer"] = float64(1)
		} else {
			r.vtables[typeID]["#has-pointer"] = float64(0)
		}

		typeIDFromName[typ.GlobalName()] = typeID

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
		tmp[i], err = r.loadMem(src.Offset(i))
		if err != nil {
			return err
		}
	}

	for i := Size(0); i < size; i++ {
		err = r.storeMem(dst.Offset(i), tmp[i])
		if err != nil {
			return err
		}
	}

	return nil
}

func (r *Runtime) memcpy(dst, src Addr, size Size) error {
	for i := Size(0); i < size; i++ {
		val, err := r.loadMem(src.Offset(i))
		if err != nil {
			return err
		}

		err = r.storeMem(dst.Offset(i), val)
		if err != nil {
			return err
		}
	}

	return nil
}

func (r *Runtime) memzero(dst Addr, size Size) error {
	for i := Size(0); i < size; i++ {
		err := r.storeMem(dst.Offset(i), 0)
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
	log.Println("GC")
	defer log.Println("GC done")

	log.Println("Scanning pointers")
	err := gc.scanPointers(r)
	if err != nil {
		return err
	}
	log.Println("Done scanning pointers")

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
		funcType, err := r.loadMem(Addr(r.funcTrace[i] + 0))
		if err != nil {
			return err
		}

		// funcName, err := r.loadMem(Addr(r.funcTrace[i] + 1))
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
			funcAddr, err := r.loadMem(Addr(r.funcTrace[i] + 3))
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

		hasPointer := r.vtables[slot.Type]["#has-pointer"] == 1
		if !hasPointer {
			continue
		}

		// val, err := r.loadMem(slot.Addr)
		// if err != nil {
		// 	return err
		// }
		//
		// name, err := r.LoadString(Addr(r.vtables[slot.Type]["#name"]))
		// if err != nil {
		// 	return err
		// }
		//
		// log.Printf("%v %s = %v", slot.Addr, string(name), val)

		switch kind {
		case kinds.Nil, kinds.Void, kinds.Int, kinds.Bool, kinds.Float:
		case kinds.String:
			val, err := r.loadMem(slot.Addr)
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
			val, err := r.loadMem(slot.Addr)
			if err != nil {
				return err
			}

			gc.addHeapVar(slot.Addr, Addr(val))

			slots = append(slots, RuntimeTypeAddr{Type: int(r.vtables[slot.Type]["#pointee"]), Addr: Addr(val)})
		case kinds.Array:
			length := int(r.vtables[slot.Type]["#length"])
			elem := int(r.vtables[slot.Type]["#elem"])
			elemSize := Size(r.vtables[elem]["#size"])
			elemHasPointer := r.vtables[elem]["#has-pointer"] == 1

			if elemHasPointer {
				for i := 0; i < length; i++ {
					slots = append(slots, RuntimeTypeAddr{Type: elem, Addr: slot.Addr.Offset(elemSize * Size(i))})
				}
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
			typ, err := r.loadMem(slot.Addr)
			if err != nil {
				return err
			}

			size := kinds.Kind(r.vtables[int(typ)]["#size"])
			if size == 0 {
				// skip
			} else if size == 1 {
				slots = append(slots, RuntimeTypeAddr{Type: int(typ), Addr: slot.Addr.Offset(1)})
			} else {
				ptrTyp := kinds.Kind(r.vtables[int(typ)]["#pointer"])

				slots = append(slots, RuntimeTypeAddr{Type: int(ptrTyp), Addr: slot.Addr.Offset(1)})
			}

		case kinds.Slice, kinds.Variadic:
			sliceData, err := r.loadMem(slot.Addr)
			if err != nil {
				return err
			}

			sliceCap, err := r.loadMem(slot.Addr + 2)
			if err != nil {
				return err
			}

			gc.addHeapVar(slot.Addr, Addr(sliceData))

			elem := int(r.vtables[slot.Type]["#elem"])
			elemSize := Size(r.vtables[elem]["#size"])
			elemKind := kinds.Kind(r.vtables[elem]["#kind"])
			elemHasPointer := r.vtables[elem]["#has-pointer"] == 1
			log.Println("slice", slot.Addr, sliceData, sliceCap, elem, elemKind, elemSize, elemHasPointer)

			if elemHasPointer {
				for i := 0; i < int(sliceCap); i++ {
					slots = append(slots, RuntimeTypeAddr{Type: elem, Addr: Addr(sliceData).Offset(elemSize * Size(i))})
				}
			}
		case kinds.Struct:
			hasPointer := r.vtables[slot.Type]["#has-pointer"] == 1
			if !hasPointer {
				continue
			}

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
			varVal, err := r.loadMem(varAddr)
			if err != nil {
				return err
			}

			log.Println("moving", alloc, varAddr, varVal, gc.heapIndex, varVal-float64(alloc))

			err = r.storeMem(varAddr, float64(gc.heapIndex)+(varVal-float64(alloc)))
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
	for _, ptr := range gc.strPtrs {
		str, err := r.LoadString(ptr)
		if err != nil {
			return err
		}
		err = r.storeStr(gc.strHeapIndex, str)
		if err != nil {
			return err
		}

		for _, v := range gc.strVars {
			val, err := r.loadMem(v)
			if err != nil {
				return err
			}

			if Addr(val) == ptr {
				err = r.storeMem(v, float64(gc.strHeapIndex))
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

func (r *Runtime) fetch(addr Addr) (float64, error) {
	page, pageAddr := r.splitAddr(addr)

	if page >= uint64(len(r.codePages)) {
		return 0, fmt.Errorf("invalid code page %v for addr %v", page, addr)
	}

	if pageAddr >= uint64(len(r.codePages[page])) {
		return 0, fmt.Errorf("invalid code page addr %v in page %v for addr %v", pageAddr, page, addr)
	}

	return r.codePages[page][pageAddr], nil
}

func (r *Runtime) loadMem(addr Addr) (float64, error) {
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
		arg, err := r.loadMem(sp.Offset(offset - size))
		if err != nil {
			return nil, err
		}

		args = append(args, arg)
	}

	return args, nil
}

func (r *Runtime) storeMem(addr Addr, val float64) error {
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
	err := env.storeMem(env.sp(), val)
	env.setSP(env.sp() + 1)
	return err
}

func (r *Runtime) pop() (float64, error) {
	val, err := r.loadMem(r.sp())
	r.setSP(r.sp() - 1)
	return val, err
}
func (r *Runtime) loadInternal(op Addr, length Addr) (float64, error) {
	var stack []float64
	var arg1 float64
	var arg2 float64
	limit := op + length

	for op < limit {
		uop, err := r.fetch(op)
		if err != nil {
			return 0, err
		}
		op++

		switch uop {
		case abc.UOPImmediate:
			val, err := r.fetch(op)
			if err != nil {
				return 0, err
			}
			op++

			stack = append(stack, val)
		case abc.UOPRegister:
			arg1, stack = stack[len(stack)-1], stack[:len(stack)-1]
			stack = append(stack, r.registers[int(arg1)])
		case abc.UOPIndirect:
			arg1, stack = stack[len(stack)-1], stack[:len(stack)-1]

			val, err := r.loadMem(Addr(arg1))
			if err != nil {
				return 0, err
			}

			stack = append(stack, val)
		case abc.UOPAddition:
			arg2, stack = stack[len(stack)-1], stack[:len(stack)-1]
			arg1, stack = stack[len(stack)-1], stack[:len(stack)-1]

			stack = append(stack, arg1+arg2)
		case abc.UOPSubtraction:
			arg2, stack = stack[len(stack)-1], stack[:len(stack)-1]
			arg1, stack = stack[len(stack)-1], stack[:len(stack)-1]

			stack = append(stack, arg1-arg2)
		case abc.UOPMultiplication:
			arg2, stack = stack[len(stack)-1], stack[:len(stack)-1]
			arg1, stack = stack[len(stack)-1], stack[:len(stack)-1]

			stack = append(stack, arg1*arg2)
		case abc.UOPEqual:
			arg2, stack = stack[len(stack)-1], stack[:len(stack)-1]
			arg1, stack = stack[len(stack)-1], stack[:len(stack)-1]

			if arg1 == arg2 {
				stack = append(stack, 1)
			} else {
				stack = append(stack, 0)
			}
		case abc.UOPNotEqual:
			arg2, stack = stack[len(stack)-1], stack[:len(stack)-1]
			arg1, stack = stack[len(stack)-1], stack[:len(stack)-1]

			if arg1 != arg2 {
				stack = append(stack, 1)
			} else {
				stack = append(stack, 0)
			}
		case abc.UOPLessThan:
			arg2, stack = stack[len(stack)-1], stack[:len(stack)-1]
			arg1, stack = stack[len(stack)-1], stack[:len(stack)-1]

			if arg1 < arg2 {
				stack = append(stack, 1)
			} else {
				stack = append(stack, 0)
			}
		case abc.UOPLessThanOrEqual:
			arg2, stack = stack[len(stack)-1], stack[:len(stack)-1]
			arg1, stack = stack[len(stack)-1], stack[:len(stack)-1]

			if arg1 <= arg2 {
				stack = append(stack, 1)
			} else {
				stack = append(stack, 0)
			}
		case abc.UOPGreaterThan:
			arg2, stack = stack[len(stack)-1], stack[:len(stack)-1]
			arg1, stack = stack[len(stack)-1], stack[:len(stack)-1]

			if arg1 > arg2 {
				stack = append(stack, 1)
			} else {
				stack = append(stack, 0)
			}
		case abc.UOPGreaterThanOrEqual:
			arg2, stack = stack[len(stack)-1], stack[:len(stack)-1]
			arg1, stack = stack[len(stack)-1], stack[:len(stack)-1]

			if arg1 >= arg2 {
				stack = append(stack, 1)
			} else {
				stack = append(stack, 0)
			}
		case abc.UOPNot:
			arg1, stack = stack[len(stack)-1], stack[:len(stack)-1]

			if arg1 == 0 {
				stack = append(stack, 1)
			} else {
				stack = append(stack, 0)
			}
		case abc.UOPBoundsCheck:
			arg2, stack = stack[len(stack)-1], stack[:len(stack)-1]
			arg1, stack = stack[len(stack)-1], stack[:len(stack)-1]

			if int(arg1) >= int(arg2) {
				return 0, fmt.Errorf("index out of bounds: %d >= %d", int(arg1), int(arg2))
			}

			stack = append(stack, arg1)
		case abc.UOPVTableLookup:
			arg2, stack = stack[len(stack)-1], stack[:len(stack)-1]
			arg1, stack = stack[len(stack)-1], stack[:len(stack)-1]

			arg2Str, err := r.LoadString(Addr(arg2))
			if err != nil {
				return 0, err
			}

			val, ok := r.vtables[int(arg1)][string(arg2Str)]
			if !ok {
				return 0, fmt.Errorf("no such vtable entry for %d %d %s", int(arg1), int(arg2), string(arg2Str))
			}

			stack = append(stack, val)
		default:
			panic(fmt.Sprintf("invalid internal opcode %d", int(uop)))
		}
	}

	return stack[0], nil
}

func (r *Runtime) load(op Addr) (float64, error) {
	length, err := r.fetch(op)
	if err != nil {
		return 0, err
	}

	return r.loadInternal(op+1, Addr(length))
}

func (r *Runtime) loadAddr(op Addr) (float64, error) {
	length, err := r.fetch(op)
	if err != nil {
		return 0, err
	}

	last, err := r.fetch(Addr(op + Addr(length)))
	if err != nil {
		return 0, err
	}

	switch last {
	case abc.UOPIndirect:
		return r.loadInternal(Addr(op+1), Addr(length-1))
	default:
		return 0, fmt.Errorf("invalid load addr uop %v", last)
	}
}

func (r *Runtime) store(op Addr, val float64) error {
	length, err := r.fetch(Addr(op))
	if err != nil {
		return err
	}

	last, err := r.fetch(op + Addr(length))
	if err != nil {
		return err
	}

	switch last {
	case abc.UOPIndirect:
		addr, err := r.loadInternal(Addr(op+1), Addr(length-1))
		if err != nil {
			return err
		}
		return r.storeMem(Addr(addr), val)
	case abc.UOPRegister:
		reg, err := r.loadInternal(Addr(op+1), Addr(length-1))
		if err != nil {
			return err
		}
		r.registers[int(reg)] = val
		return nil
	default:
		return fmt.Errorf("invalid store uop %v", last)
	}
}

func (r *Runtime) panic(err error) error {
	fmt.Fprintf(r.stdout, "panic: %v\n\n", err)
	for i := len(r.funcTrace) - 1; i >= 1; i -= 2 {
		funcAddr := Addr(r.funcTrace[i])

		funcNameAddr, err := r.loadMem(funcAddr + 1)
		if err != nil {
			return err
		}

		funcName, err := r.LoadString(Addr(funcNameAddr))
		if err != nil {
			return err
		}

		funcFileAddr, err := r.loadMem(funcAddr + 2)
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
	pc, err := r.loadMem(funcAddr + 3)
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
			// log.Println(abc.Compile(r.prog.Instructions())[r.pc():])
			log.Printf("%v %v", code, r.pc())
			// log.Printf("fp: %v", r.fp())
			// log.Printf("sp: %v", r.sp())
			// log.Printf("pc: %v", r.pc())
			// r.printRegisters()
			// r.printStack()
		}

		switch code {
		case abc.Nop:
			r.setPC(r.pc() + 1)
		case abc.Mov:
			size, err := r.fetch(r.pc() + 1)
			if err != nil {
				return err
			}

			dstLen, err := r.fetch(r.pc() + 2)
			if err != nil {
				return err
			}

			srcLen, err := r.fetch(r.pc() + 3 + Addr(dstLen))
			if err != nil {
				return err
			}

			if size == 1 {
				val, err := r.load(r.pc() + 3 + Addr(dstLen))
				if err != nil {
					return err
				}

				err = r.store(r.pc()+2, val)
				if err != nil {
					return err
				}
			} else {
				srcAddr, err := r.loadAddr(r.pc() + 3 + Addr(dstLen))
				if err != nil {
					return err
				}

				dstAddr, err := r.loadAddr(r.pc() + 2)
				if err != nil {
					return err
				}

				err = r.memmove(Addr(dstAddr), Addr(srcAddr), Size(size))
				if err != nil {
					return err
				}
			}
			r.setPC(r.pc() + 4 + Addr(dstLen) + Addr(srcLen))
		case abc.Ret:
			fp := r.fp()

			size, err := r.fetch(r.pc() + 1)
			if err != nil {
				return err
			}

			for reg := range r.registers {
				r.registers[reg], err = r.loadMem(fp.Offset(-air.Size(reg)))
				if err != nil {
					return fmt.Errorf("failed to push %s", Register(reg))
				}
			}

			r.setSP(r.sp().Offset(-Size(size)))

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
		case abc.Call:
			line, err := r.fetch(r.pc() + 1)
			if err != nil {
				return fmt.Errorf("could not fetch call line number: %w", err)
			}

			funcInfoAddrLen, err := r.fetch(r.pc() + 2)
			if err != nil {
				return fmt.Errorf("could not fetch function info addr len: %w", err)
			}

			funcInfoAddr, err := r.load(r.pc() + 2)
			if err != nil {
				return fmt.Errorf("could not resolve function info: %w", err)
			}

			funcType, err := r.loadMem(Addr(funcInfoAddr))
			if err != nil {
				return fmt.Errorf("could not resolve function addr: %w", err)
			}

			if r.debug {
				fNameAddr, err := r.loadMem(Addr(funcInfoAddr) + 1)
				if err != nil {
					return err
				}

				fName, err := r.LoadString(Addr(fNameAddr))
				if err != nil {
					return err
				}

				log.Printf("call %s", string(fName))
			}

			r.funcTrace = append(r.funcTrace, line, funcInfoAddr)

			switch int(funcType) {
			case RuntimeFuncTypeNil:
				return fmt.Errorf("tried to call nil function reference")
			case RuntimeFuncTypeExtern:
				externName, err := r.loadMem(Addr(funcInfoAddr) + 3)
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
					return fmt.Errorf("failed to load %d args for extern", entry.ArgSize)
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
					r.storeMem(r.sp().Offset(-(entry.ArgSize + entry.ReturnSize)).Offset(-i), retArgs[i])
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
				r.setPC(r.pc() + 3 + Addr(funcInfoAddrLen))

				continue
			case RuntimeFuncTypeFunc:
				faddr, err := r.loadMem(Addr(funcInfoAddr) + 3)
				if err != nil {
					return fmt.Errorf("could not resolve function addr")
				}

				frameSize := Addr(len(r.registers))

				r.setPC(r.pc() + 3 + Addr(funcInfoAddrLen))

				for reg := range Register(len(r.registers)) {
					err := r.storeMem(r.sp()+frameSize-Addr(reg)-1, r.registers[reg])
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
		case abc.Jmp:
			targetLen, err := r.fetch(r.pc() + 1)
			if err != nil {
				return err
			}

			target, err := r.load(r.pc() + 1)
			if err != nil {
				return err
			}

			condLen, err := r.fetch(r.pc() + 2 + Addr(targetLen))
			if err != nil {
				return err
			}

			cond, err := r.load(r.pc() + 2 + Addr(targetLen))
			if err != nil {
				return err
			}

			if cond != 0 {
				r.setPC(Addr(target))
			} else {
				r.setPC(r.pc() + 3 + Addr(targetLen) + Addr(condLen))
			}
		case abc.Binop:
			dstLen, err := r.fetch(r.pc() + 1)
			if err != nil {
				return err
			}

			kind, err := r.fetch(r.pc() + 2 + Addr(dstLen))
			if err != nil {
				return err
			}

			op, err := r.fetch(r.pc() + 3 + Addr(dstLen))
			if err != nil {
				return err
			}

			leftLen, err := r.fetch(r.pc() + 4 + Addr(dstLen))
			if err != nil {
				return err
			}

			left, err := r.load(r.pc() + 4 + Addr(dstLen))
			if err != nil {
				return err
			}

			rightLen, err := r.fetch(r.pc() + 5 + Addr(dstLen) + Addr(leftLen))
			if err != nil {
				return err
			}

			right, err := r.load(r.pc() + 5 + Addr(dstLen) + Addr(leftLen))
			if err != nil {
				return err
			}

			switch op {
			case abc.UOPAddition:
				switch kinds.Kind(kind) {
				case kinds.Int:
					err = r.store(r.pc()+1, left+right)
					if err != nil {
						return err
					}
				case kinds.Float:
					err = r.store(r.pc()+1, left+right)
					if err != nil {
						return err
					}
				case kinds.String:
					leftStr, err := r.LoadString(Addr(left))
					if err != nil {
						return err
					}

					rightStr, err := r.LoadString(Addr(right))
					if err != nil {
						return err
					}

					val, err := r.allocStr(leftStr + rightStr)
					if err != nil {
						return err
					}

					err = r.store(r.pc()+1, float64(val))
					if err != nil {
						return err
					}
				default:
					return fmt.Errorf("unhandled binary operation %v", kinds.Kind(kind))
				}
			case abc.UOPSubtraction:
				switch kinds.Kind(kind) {
				case kinds.Int:
					err = r.store(r.pc()+1, left-right)
					if err != nil {
						return err
					}
				case kinds.Float:
					err = r.store(r.pc()+1, left-right)
					if err != nil {
						return err
					}
				default:
					return fmt.Errorf("unhandled binary operation %v", kinds.Kind(kind))
				}
			case abc.UOPMultiplication:
				switch kinds.Kind(kind) {
				case kinds.Int:
					err = r.store(r.pc()+1, left*right)
					if err != nil {
						return err
					}
				case kinds.Float:
					err = r.store(r.pc()+1, left*right)
					if err != nil {
						return err
					}
				default:
					return fmt.Errorf("unhandled binary operation %v", kinds.Kind(kind))
				}
			case abc.UOPDivision:
				switch kinds.Kind(kind) {
				case kinds.Int:
					err = r.store(r.pc()+1, float64(int64(left)/int64(right)))
					if err != nil {
						return err
					}
				case kinds.Float:
					err = r.store(r.pc()+1, left/right)
					if err != nil {
						return err
					}
				default:
					return fmt.Errorf("unhandled binary operation %v", kinds.Kind(kind))
				}
			case abc.UOPModulo:
				switch kinds.Kind(kind) {
				case kinds.Int:
					err = r.store(r.pc()+1, float64(int64(left)%int64(right)))
					if err != nil {
						return err
					}
				default:
					return fmt.Errorf("unhandled binary operation %v", kinds.Kind(kind))
				}
			case abc.UOPLogicalAnd:
				switch kinds.Kind(kind) {
				case kinds.Bool:
					var val float64
					if left != 0 && right != 0 {
						val = 1
					} else {
						val = 0
					}
					err = r.store(r.pc()+1, val)
					if err != nil {
						return err
					}
				default:
					return fmt.Errorf("unhandled binary operation %v", kinds.Kind(kind))
				}
			case abc.UOPLogicalOr:
				switch kinds.Kind(kind) {
				case kinds.Bool:
					var val float64
					if left != 0 || right != 0 {
						val = 1
					} else {
						val = 0
					}
					err = r.store(r.pc()+1, val)
					if err != nil {
						return err
					}
				default:
					return fmt.Errorf("unhandled binary operation %v", kinds.Kind(kind))
				}
			case abc.UOPEqual:
				switch kinds.Kind(kind) {
				case kinds.Type:
					err = r.store(r.pc()+1, b(left == right))
					if err != nil {
						return err
					}
				case kinds.Pointer:
					err = r.store(r.pc()+1, b(left == right))
					if err != nil {
						return err
					}
				case kinds.Bool:
					err = r.store(r.pc()+1, b(left == right))
					if err != nil {
						return err
					}
				case kinds.Int:
					err = r.store(r.pc()+1, b(left == right))
					if err != nil {
						return err
					}
				case kinds.Float:
					err = r.store(r.pc()+1, b(left == right))
					if err != nil {
						return err
					}
				case kinds.String:
					leftStr, err := r.LoadString(Addr(left))
					if err != nil {
						return err
					}

					rightStr, err := r.LoadString(Addr(right))
					if err != nil {
						return err
					}

					err = r.store(r.pc()+1, b(leftStr == rightStr))
					if err != nil {
						return err
					}
				default:
					return fmt.Errorf("unhandled binary operation %v", kinds.Kind(kind))
				}
			case abc.UOPNotEqual:
				switch kinds.Kind(kind) {
				case kinds.Type:
					err = r.store(r.pc()+1, b(left != right))
					if err != nil {
						return err
					}
				case kinds.Pointer:
					err = r.store(r.pc()+1, b(left != right))
					if err != nil {
						return err
					}
				case kinds.Bool:
					err = r.store(r.pc()+1, b(left != right))
					if err != nil {
						return err
					}
				case kinds.Int:
					err = r.store(r.pc()+1, b(left != right))
					if err != nil {
						return err
					}
				case kinds.Float:
					err = r.store(r.pc()+1, b(left != right))
					if err != nil {
						return err
					}
				case kinds.String:
					leftStr, err := r.LoadString(Addr(left))
					if err != nil {
						return err
					}

					rightStr, err := r.LoadString(Addr(right))
					if err != nil {
						return err
					}

					err = r.store(r.pc()+1, b(leftStr != rightStr))
					if err != nil {
						return err
					}
				default:
					return fmt.Errorf("unhandled binary operation %v", kinds.Kind(kind))
				}
			default:
				return fmt.Errorf("unhandled binary operation %v", op)
			}

			r.setPC(r.pc() + 6 + Addr(dstLen) + Addr(leftLen) + Addr(rightLen))
		case abc.Unop:
			dstLen, err := r.fetch(r.pc() + 1)
			if err != nil {
				return err
			}

			kind, err := r.fetch(r.pc() + 2 + Addr(dstLen))
			if err != nil {
				return err
			}

			op, err := r.fetch(r.pc() + 3 + Addr(dstLen))
			if err != nil {
				return err
			}

			argLen, err := r.fetch(r.pc() + 4 + Addr(dstLen))
			if err != nil {
				return err
			}

			arg, err := r.load(r.pc() + 4 + Addr(dstLen))
			if err != nil {
				return err
			}

			switch op {
			case abc.UOPNot:
				switch kinds.Kind(kind) {
				case kinds.Bool:
					if arg == 0 {
						err = r.store(r.pc()+1, 1)
						if err != nil {
							return err
						}
					} else {
						err = r.store(r.pc()+1, 0)
						if err != nil {
							return err
						}
					}
				default:
					return fmt.Errorf("unhandled unary operation %v", kinds.Kind(kind))
				}
			case abc.UOPNegate:
				switch kinds.Kind(kind) {
				case kinds.Int:
					err = r.store(r.pc()+1, -arg)
					if err != nil {
						return err
					}
				case kinds.Float:
					err = r.store(r.pc()+1, -arg)
					if err != nil {
						return err
					}
				default:
					return fmt.Errorf("unhandled unary operation %v", kinds.Kind(kind))
				}
			default:
				return fmt.Errorf("unhandled unary operation %v", op)
			}

			r.setPC(r.pc() + 5 + Addr(dstLen) + Addr(argLen))
		case abc.Alc:
			dstLen, err := r.fetch(r.pc() + 1)
			if err != nil {
				return err
			}

			sizeLen, err := r.fetch(r.pc() + 2 + Addr(dstLen))
			if err != nil {
				return err
			}

			size, err := r.load(r.pc() + 2 + Addr(dstLen))
			if err != nil {
				return err
			}

			addr, err := r.alloc(Size(size))
			if err != nil {
				return err
			}

			err = r.store(r.pc()+1, float64(addr))
			if err != nil {
				return err
			}

			r.setPC(r.pc() + 3 + Addr(dstLen) + Addr(sizeLen))
		case abc.App:
			dstLen, err := r.fetch(r.pc() + 1)
			if err != nil {
				return err
			}

			dst, err := r.loadAddr(r.pc() + 1)
			if err != nil {
				return err
			}

			srcLen, err := r.fetch(r.pc() + 2 + Addr(dstLen))
			if err != nil {
				return err
			}

			src, err := r.loadAddr(r.pc() + 2 + Addr(dstLen))
			if err != nil {
				return err
			}

			elemLen, err := r.fetch(r.pc() + 3 + Addr(dstLen) + Addr(srcLen))
			if err != nil {
				return err
			}

			size, err := r.fetch(r.pc() + 4 + Addr(dstLen) + Addr(srcLen) + Addr(elemLen))
			if err != nil {
				return err
			}

			sliceData, err := r.loadMem(Addr(src))
			if err != nil {
				return err
			}
			sliceDataAddr := Addr(sliceData)

			sliceLen, err := r.loadMem(Addr(src + 1))
			if err != nil {
				return err
			}

			sliceCap, err := r.loadMem(Addr(src + 2))
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

				newDataAddr, err := r.alloc(Size(sliceCap) * Size(size))
				if err != nil {
					return err
				}

				err = r.memcpy(newDataAddr, sliceDataAddr, Size(sliceLen)*Size(size))
				if err != nil {
					return err
				}

				sliceDataAddr = newDataAddr
			}

			if size == 1 {
				elem, err := r.load(r.pc() + 3 + Addr(dstLen) + Addr(srcLen))
				if err != nil {
					return err
				}

				err = r.storeMem(sliceDataAddr.Offset(Size(sliceLen)*Size(size)), elem)
				if err != nil {
					return err
				}
			} else {
				elem, err := r.loadAddr(r.pc() + 3 + Addr(dstLen) + Addr(srcLen))
				if err != nil {
					return err
				}

				err = r.memcpy(sliceDataAddr.Offset(Size(sliceLen)*Size(size)), Addr(elem), Size(size))
				if err != nil {
					return err
				}
			}

			err = r.storeMem(Addr(dst), float64(sliceDataAddr))
			if err != nil {
				return err
			}

			err = r.storeMem(Addr(dst+1), float64(sliceLen+1))
			if err != nil {
				return err
			}

			err = r.storeMem(Addr(dst+2), float64(sliceCap))
			if err != nil {
				return err
			}

			r.setPC(r.pc() + 5 + Addr(dstLen) + Addr(srcLen) + Addr(elemLen))
		default:
			return fmt.Errorf("unrecognized bytecode: %T %v", code, code)
		}

		if err != nil {
			return err
		}
	}
}

type Float = air.Float
type Int = air.Int
type String = air.String
type Bool = air.Bool
type Addr = air.Addr
type Size = air.Size
type Register = air.Register

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
		val, err := r.loadMem(addr)
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

func b(b bool) float64 {
	if b {
		return 1
	}
	return 0
}
