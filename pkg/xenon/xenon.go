package xenon

import (
	_ "embed"
	"fmt"
	"io"
	"log"
	"text/template"

	"github.com/rhino1998/aeon/pkg/compiler"
)

//go:embed main.xc.tmpl
var tmplText string

type PageAddr struct {
	Page int
	Addr int
}

type ExternFuncEntry struct {
	ArgTypes  []string
	HasReturn bool
	Name      string
}

type ExternFuncs []ExternFuncEntry

type xenonContext struct {
	PageSize     int
	NumCodePages int
	Code         map[PageAddr]string
	VarInitFunc  int
	InitFuncs    []int
	UpdateFuncs  []int

	ExternFuncs  []ExternFuncEntry
	NumMemPages  int
	NumRegisters int
	MaxLoadDepth int
	Debug        bool
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

func EmitXenonCode(w io.Writer, prog *compiler.Program) error {
	var xeCtx xenonContext
	xeCtx.PageSize = PageSize
	xeCtx.NumCodePages = (len(prog.Bytecode()) + xeCtx.PageSize) / xeCtx.PageSize
	xeCtx.NumMemPages = 10
	xeCtx.NumRegisters = 16
	xeCtx.MaxLoadDepth = 3

	var err error
	varinitFunc, err := getFunc(prog, "main", "varinit")
	if err != nil {
		return err
	}

	xeCtx.VarInitFunc = int(varinitFunc.Addr())
	for _, f := range prog.InitFunctions() {
		xeCtx.InitFuncs = append(xeCtx.InitFuncs, int(f.Addr()))
	}

	for _, f := range prog.UpdateFunctions() {
		xeCtx.UpdateFuncs = append(xeCtx.UpdateFuncs, int(f.Addr()))
	}

	xeCtx.Code = make(map[PageAddr]string)

	log.Printf("Program BC:%d PageSize:%d", len(prog.Bytecode()), xeCtx.PageSize)

	for _, extern := range prog.ExternFuncs() {
		ftype := extern.Type().(*compiler.FunctionType)
		argTypes := make([]string, 0)
		for _, param := range ftype.Parameters {
			var argType string
			if param.Kind() == compiler.KindInt || param.Kind() == compiler.KindBool || param.Kind() == compiler.KindFloat {
				argType = ":number"
			}

			argTypes = append(argTypes, argType)
		}
		xeCtx.ExternFuncs = append(xeCtx.ExternFuncs, ExternFuncEntry{
			ArgTypes:  argTypes,
			HasReturn: ftype.Return.Kind() != compiler.KindVoid,
			Name:      extern.Name(),
		})
	}

	for i, bc := range prog.Bytecode() {
		bcBytes, err := marshalByteCode(bc)
		if err != nil {
			return err
		}

		page := i / xeCtx.PageSize
		pageAddr := i % xeCtx.PageSize

		log.Printf("%d:%d:%s", page, pageAddr, bc)
		xeCtx.Code[PageAddr{
			Page: page,
			Addr: pageAddr}] = string(bcBytes)
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
	}

	tmpl, err := template.New("main.xc.tmpl").Funcs(funcMap).Parse(tmplText)
	if err != nil {
		return err
	}

	return tmpl.Execute(w, &xeCtx)
}

func marshalByteCode(bc compiler.Bytecode) ([]byte, error) {
	code := bc.Name()

	// TODO: better format

	args, err := MarshalXenon(bc)
	if err != nil {
		return nil, fmt.Errorf("failed to marshal bytecode %s: %w", code, err)
	}

	return []byte(fmt.Sprintf(".t{%s}%s", code, args)), nil
}
