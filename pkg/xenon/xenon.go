package xenon

import (
	"context"
	_ "embed"
	"fmt"
	"io"
	"log"
	"log/slog"
	"text/template"

	"github.com/rhino1998/aeon/pkg/compiler"
	"github.com/rhino1998/aeon/pkg/compiler/abc"
	"github.com/rhino1998/aeon/pkg/compiler/air"
	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/types"
)

//go:embed main.xc.tmpl
var tmplText string

type PageAddr struct {
	Page int
	Addr int
}

type ArgWrapper struct {
	Prefix string
	Suffix string
}

type ExternFuncEntry struct {
	ArgTypes  []ArgWrapper
	Size      int
	HasReturn bool
	Name      string
}

type ExternFuncs []ExternFuncEntry

type VTable map[int]VTableTypeEntry

type VTableTypeEntry struct {
	Name string
	Data map[string]int
}

type xenonContext struct {
	PageSize     int
	NumCodePages int
	Code         map[PageAddr]string
	GlobalLayout []int
	StackLayouts map[int][]int
	VarInitFuncs []int
	InitFuncs    []int
	UpdateFuncs  []int

	ExternFuncs  []ExternFuncEntry
	Strings      []string
	NumMemPages  int
	NumStrPages  int
	NumRegisters int
	GlobalSize   int
	MaxLoadDepth int
	Debug        bool

	VTable VTable

	OPSep  string
	UOPSep string

	KindNil       int
	KindInt       int
	KindFloat     int
	KindBool      int
	KindString    int
	KindPointer   int
	KindType      int
	KindTuple     int
	KindArray     int
	KindStruct    int
	KindSlice     int
	KindVariadic  int
	KindMap       int
	KindInterface int
}

func getFunc(prog *compiler.Program, pkgName, funcName string) (*compiler.Function, error) {
	pkg, ok := prog.Package(pkgName)
	if !ok {
		return nil, fmt.Errorf("failed to get package %q", pkgName)
	}
	f, ok := pkg.Function(funcName)
	if !ok {
		return nil, fmt.Errorf("failed to get function %q from package %q", funcName, pkgName)
	}

	return f, nil
}

func EmitXenonCode(ctx context.Context, logger *slog.Logger, w io.Writer, prog *compiler.Program) error {
	abcProg := abc.Compile(prog.Instructions())

	var xeCtx xenonContext
	xeCtx.PageSize = 1000
	xeCtx.NumCodePages = (abcProg.Bytecode.Length() + xeCtx.PageSize) / xeCtx.PageSize
	xeCtx.NumMemPages = 1
	xeCtx.NumStrPages = 1
	xeCtx.NumRegisters = prog.Registers()
	xeCtx.MaxLoadDepth = 5
	xeCtx.GlobalSize = int(prog.GlobalSize())
	xeCtx.VTable = make(VTable)
	xeCtx.Debug = logger.Handler().Enabled(ctx, slog.LevelDebug)
	xeCtx.OPSep = "\x9B"

	xeCtx.KindNil = int(kinds.Nil)
	xeCtx.KindInt = int(kinds.Int)
	xeCtx.KindFloat = int(kinds.Float)
	xeCtx.KindBool = int(kinds.Bool)
	xeCtx.KindString = int(kinds.String)
	xeCtx.KindPointer = int(kinds.Pointer)
	xeCtx.KindType = int(kinds.Type)
	xeCtx.KindTuple = int(kinds.Tuple)
	xeCtx.KindArray = int(kinds.Array)
	xeCtx.KindStruct = int(kinds.Struct)
	xeCtx.KindSlice = int(kinds.Slice)
	xeCtx.KindVariadic = int(kinds.Variadic)
	xeCtx.KindMap = int(kinds.Map)
	xeCtx.KindInterface = int(kinds.Interface)

	strMap := make(map[air.String]int)
	for i, str := range prog.Strings() {
		xeCtx.Strings = append(xeCtx.Strings, string(str))
		strMap[str] = i
	}

	for _, f := range prog.VarInitFunctions() {
		xeCtx.VarInitFuncs = append(xeCtx.VarInitFuncs, int(f.InfoAddr()))
	}

	for _, f := range prog.InitFunctions() {
		xeCtx.InitFuncs = append(xeCtx.InitFuncs, int(f.InfoAddr()))
	}

	for _, f := range prog.UpdateFunctions() {
		xeCtx.UpdateFuncs = append(xeCtx.UpdateFuncs, int(f.InfoAddr()))
	}

	allFuncs := make(map[string]int)

	for _, fun := range prog.AllFunctions() {
		allFuncs[fun.QualifiedName()] = int(fun.InfoAddr())
	}

	typeIDFromName := make(map[types.Name]int)
	for i, typ := range prog.Types() {
		typeIDFromName[typ.GlobalName()] = i
		xeCtx.VTable[i] = VTableTypeEntry{
			Name: string(typ.GlobalName()),
			Data: make(map[string]int),
		}
		typSize, err := air.TypeSize(typ)
		if err != nil {
			return fmt.Errorf("failed to get type size: %w", err)
		}
		xeCtx.VTable[i].Data["#kind"] = int(typ.Kind())
		xeCtx.VTable[i].Data["#size"] = int(typSize)
		xeCtx.VTable[i].Data["#name"] = int(strMap[air.String(typ.GlobalName())])
		switch typ := typ.(type) {
		case *types.Derived:
			for _, method := range typ.Methods(false) {
				methodName := typ.MethodQualifiedName(false, method.Name)
				funAddr, ok := allFuncs[methodName]
				if !ok {
					return fmt.Errorf("method %s not found", methodName)
				}
				xeCtx.VTable[i].Data[method.Name] = funAddr
			}
		case *types.Pointer:
			switch typ := types.Dereference(typ.Pointee()).(type) {
			case *types.Derived:
				for _, method := range typ.Methods(true) {
					methodName := typ.MethodQualifiedName(true, method.Name)
					funAddr, ok := allFuncs[methodName]
					if !ok {
						return fmt.Errorf("method %s not found", methodName)
					}
					xeCtx.VTable[i].Data[method.Name] = funAddr
				}
			}
		}
	}

	for typeID, typ := range prog.Types() {
		entry := xeCtx.VTable[typeID]
		typSize, err := air.TypeSize(typ)
		if err != nil {
			return fmt.Errorf("failed to get type size: %w", err)
		}

		hasPointer := types.HasPointer(typ)
		if hasPointer {
			entry.Data["#has-pointer"] = 1
		} else {
			entry.Data["#has-pointer"] = 0
		}

		if typSize > 1 {
			entry.Data["#pointer"] = typeIDFromName[types.NewPointer(typ).GlobalName()]
		}

		switch typ := types.Resolve(typ).(type) {
		case *types.Pointer:
			xeCtx.VTable[typeID].Data["#pointee"] = int(typeIDFromName[typ.Pointee().GlobalName()])
		case *types.Array:
			xeCtx.VTable[typeID].Data["#length"] = int(typ.Length())
			xeCtx.VTable[typeID].Data["#elem"] = int(typeIDFromName[typ.Elem().GlobalName()])
		case *types.Tuple:
			xeCtx.VTable[typeID].Data["#elems"] = int(len(typ.Elems()))
			for i, elem := range typ.Elems() {
				xeCtx.VTable[typeID].Data[fmt.Sprintf("#elem.%d", i)] = int(typeIDFromName[elem.GlobalName()])
			}
		case *types.Slice:
			xeCtx.VTable[typeID].Data["#elem"] = int(typeIDFromName[typ.Elem().GlobalName()])
		case *types.Variadic:
			xeCtx.VTable[typeID].Data["#elem"] = int(typeIDFromName[typ.Elem().GlobalName()])
		case *types.Struct:
			xeCtx.VTable[typeID].Data["#fields"] = int(len(typ.Fields()))
			for i, field := range typ.Fields() {
				xeCtx.VTable[typeID].Data[fmt.Sprintf("#field.%d", i)] = int(typeIDFromName[field.Type.GlobalName()])
				xeCtx.VTable[typeID].Data[fmt.Sprintf("#field.%d.name", i)] = int(strMap[String(field.Name)])
			}
		}
	}

	xeCtx.GlobalLayout = make([]int, 0, xeCtx.GlobalSize)
	for _, typ := range prog.GlobalLayout() {
		typeID, ok := typeIDFromName[typ.GlobalName()]
		if !ok {
			return fmt.Errorf("type %s not found", typ.GlobalName())
		}

		xeCtx.GlobalLayout = append(xeCtx.GlobalLayout, typeID)
	}

	xeCtx.StackLayouts = make(map[int][]int)

	// TODO: maybe use normal tuple types as stack layouts?
	for _, f := range prog.AllFunctions() {
		layout, err := f.StackLayout()
		if err != nil {
			return err
		}

		flatLayout := make([]int, 0, len(layout))
		for _, slot := range layout {
			typeID, ok := typeIDFromName[slot.Type.GlobalName()]
			if !ok {
				return fmt.Errorf("type %s not found", slot.Type.GlobalName())
			}
			flatLayout = append(flatLayout, typeID)
		}

		addr, ok := abcProg.Labels[air.Label(f.QualifiedName())]
		if !ok {
			return fmt.Errorf("function label %s not found", f.QualifiedName())
		}

		xeCtx.StackLayouts[int(addr)] = flatLayout
	}

	xeCtx.Code = make(map[PageAddr]string)

	logger.Debug("Program",
		slog.Int("page_size", xeCtx.PageSize),
		slog.Int("code_pages", xeCtx.NumCodePages),
		slog.Int("mem_pages", xeCtx.NumMemPages),
		slog.Int("str_pages", xeCtx.NumStrPages),
		slog.Int("instructions", len(prog.Instructions())),
	)

	for _, extern := range prog.ExternFuncs() {
		ftype := extern.Type().(*types.Function)
		argTypes := make([]ArgWrapper, 0)
		var size Size
		retSize, err := air.TypeSize(ftype.Return)
		if err != nil {
			return fmt.Errorf("failed to get return size: %w", err)
		}
		size += retSize

		flatKinds, err := extern.FlatParameterKinds()
		if err != nil {
			return fmt.Errorf("failed to get flat parameter kinds: %w", err)
		}

		for _, kind := range flatKinds {
			var wrapper ArgWrapper
			if kind == kinds.String {
				wrapper.Prefix = "@aeon_str_load("
				wrapper.Suffix = ")"
			}

			size += 1

			argTypes = append(argTypes, wrapper)
		}
		xeCtx.ExternFuncs = append(xeCtx.ExternFuncs, ExternFuncEntry{
			ArgTypes:  argTypes,
			HasReturn: ftype.Return.Kind() != kinds.Void,
			Size:      int(size),
			Name:      extern.Name(),
		})
	}

	log.Println(abcProg.Labels)

	var codeAddr int
	for i, ins := range abcProg.Bytecode {
		page := codeAddr / xeCtx.PageSize
		pageAddr := codeAddr % xeCtx.PageSize
		logger.Debug("debug: %d:%d:%s", slog.Int("page", page), slog.Int("pageAddr", pageAddr), slog.Any("instruction", ins))
		log.Printf("%d: %s", codeAddr, prog.Instructions()[i])
		for _, uop := range ins {
			page := codeAddr / xeCtx.PageSize
			pageAddr := codeAddr % xeCtx.PageSize

			xeCtx.Code[PageAddr{
				Page: page,
				Addr: pageAddr}] = fmt.Sprintf("%f", uop)
			codeAddr++
		}
	}

	funcMap := template.FuncMap{
		"loop": func(from, to int) []int {
			var ret []int
			for i := from; i < to; i++ {
				ret = append(ret, i)
			}
			return ret
		},
		"add": func(a, b int) int {
			return a + b
		},
		"sub": func(a, b int) int {
			return a - b
		},
		"mul": func(a, b int) int {
			return a * b
		},
		"div": func(a, b int) int {
			return a / b
		},
	}

	tmpl, err := template.New("main.xc.tmpl").Funcs(funcMap).Parse(tmplText)
	if err != nil {
		return err
	}

	return tmpl.Execute(w, &xeCtx)
}
