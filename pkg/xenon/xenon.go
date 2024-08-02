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

type PageAddr struct {
	Page int
	Addr int
}

type xenonContext struct {
	PageSize     int
	NumCodePages int
	Code         map[PageAddr]string
	MainFunc     int

	NumMemPages  int
	NumRegisters int
}

func EmitXenonCode(w io.Writer, bcs []Bytecode, funcs map[string]map[string]Addr) error {
	var xeCtx xenonContext
	xeCtx.PageSize = PageSize
	xeCtx.NumCodePages = (len(bcs) + xeCtx.PageSize) / xeCtx.PageSize
	xeCtx.NumMemPages = 10
	xeCtx.NumRegisters = 16
	xeCtx.MainFunc = int(funcs["main"]["main"])
	xeCtx.Code = make(map[PageAddr]string)

	log.Printf("Program BC:%d PageSize:%d", len(bcs), xeCtx.PageSize)

	for i, bc := range bcs {
		bcBytes, err := marshalByteCode(bc)
		if err != nil {
			return err
		}

		page := i / xeCtx.PageSize
		pageAddr := i % xeCtx.PageSize

		log.Printf("%d:%d:%s", page, pageAddr, bcBytes)
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

func marshalByteCode(bc Bytecode) ([]byte, error) {
	code := bc.xenon()

	args, err := MarshalXenon(bc)
	if err != nil {
		return nil, fmt.Errorf("failed to marshal bytecode %s: %w", code, err)
	}

	return []byte(fmt.Sprintf(".t{%s}%s", code, args)), nil
}
