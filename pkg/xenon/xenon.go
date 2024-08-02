package xenon

import (
	_ "embed"
	"fmt"
	"io"
	"log"
	"text/template"
)

//go:embed main.xc.tmpl
var tmplText string

type xenonContext struct {
	PageSize     int
	NumCodePages int
	Code         []string

	NumMemPages  int
	NumRegisters int
}

func EmitXenonCode(w io.Writer, bcs []Bytecode) error {
	var xeCtx xenonContext
	xeCtx.PageSize = PageSize
	xeCtx.NumCodePages = (len(bcs) + xeCtx.PageSize) / xeCtx.PageSize
	xeCtx.NumMemPages = 200
	xeCtx.NumRegisters = 16

	for _, bc := range bcs {
		bcBytes, err := marshalByteCode(bc)
		if err != nil {
			return err
		}

		log.Printf("%s", bcBytes)
		xeCtx.Code = append(xeCtx.Code, string(bcBytes))
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

func marshalByteCode(bc Bytecode) ([]byte, error) {
	code := bc.xenon()

	args, err := MarshalXenon(bc)
	if err != nil {
		return nil, fmt.Errorf("failed to marshal bytecode %s: %w", code, err)
	}

	return []byte(fmt.Sprintf(".t{%s}%s", code, args)), nil
}
