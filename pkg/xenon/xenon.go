package xenon

import (
	"context"
	_ "embed"
	"fmt"
	"io"
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

type VTable map[int]map[string]int

type xenonContext struct {
	PageSize     int
	NumCodePages int
	Code         map[PageAddr]float64
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

	KindNil     int
	KindInt     int
	KindFloat   int
	KindBool    int
	KindString  int
	KindPointer int
	KindType    int
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
	var xeCtx xenonContext
	xeCtx.PageSize = 65535
	xeCtx.NumCodePages = (len(prog.Instructions()) + xeCtx.PageSize) / xeCtx.PageSize
	xeCtx.NumMemPages = 1
	xeCtx.NumStrPages = 1
	xeCtx.NumRegisters = prog.Registers()
	xeCtx.MaxLoadDepth = 5
	globalSize, err := prog.GlobalSize()
	if err != nil {
		return err
	}
	xeCtx.GlobalSize = int(globalSize)
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

	for _, str := range prog.Strings() {
		xeCtx.Strings = append(xeCtx.Strings, string(str))
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

	for i, typ := range prog.Types() {
		xeCtx.VTable[i] = make(map[string]int)
		typSize, err := air.TypeSize(typ)
		if err != nil {
			return fmt.Errorf("failed to get type size: %w", err)
		}
		xeCtx.VTable[i]["#size"] = int(typSize)
		xeCtx.VTable[i]["#kind"] = int(typ.Kind())
		switch typ := typ.(type) {
		case *types.Derived:
			for _, method := range typ.Methods(false) {
				methodName := typ.MethodQualifiedName(false, method.Name)
				funAddr, ok := allFuncs[methodName]
				if !ok {
					return fmt.Errorf("method %s not found", methodName)
				}
				xeCtx.VTable[i][method.Name] = funAddr
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
					xeCtx.VTable[i][method.Name] = funAddr
				}
			}
		}
	}

	xeCtx.Code = make(map[PageAddr]float64)

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

	bytecode := abc.Compile(prog.Instructions())

	for i, bc := range bytecode {
		page := i / xeCtx.PageSize
		pageAddr := i % xeCtx.PageSize

		logger.Debug("debug: %d:%d:%s", slog.Int("page", page), slog.Int("pageAddr", pageAddr), slog.Any("bc", bc))
		xeCtx.Code[PageAddr{
			Page: page,
			Addr: pageAddr}] = bc
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
