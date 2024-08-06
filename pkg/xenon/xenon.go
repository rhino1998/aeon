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
	MainFunc     int

	ExternFuncs  []ExternFuncEntry
	NumMemPages  int
	NumRegisters int
}

func EmitXenonCode(w io.Writer, prog *compiler.Program) error {
	var xeCtx xenonContext
	xeCtx.PageSize = PageSize
	xeCtx.NumCodePages = (len(prog.Bytecode()) + xeCtx.PageSize) / xeCtx.PageSize
	xeCtx.NumMemPages = 10
	xeCtx.NumRegisters = 16

	pkg, ok := prog.Package("main")
	if !ok {
		return fmt.Errorf("failed to get entry point")
	}
	f, ok := pkg.Function("main")
	if !ok {
		return fmt.Errorf("failed to get entry point")
	}

	xeCtx.MainFunc = int(f.Addr())
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
			HasReturn: ftype.Return != nil,
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
