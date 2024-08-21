package xenon

import (
	"bytes"
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
	Code         map[PageAddr]string
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

func EmitXenonCode(w io.Writer, prog *compiler.Program, debug bool) error {
	var xeCtx xenonContext
	xeCtx.PageSize = 65535
	xeCtx.NumCodePages = (len(prog.Bytecode()) + xeCtx.PageSize) / xeCtx.PageSize
	xeCtx.NumMemPages = 10
	xeCtx.NumStrPages = 10
	xeCtx.NumRegisters = prog.Registers()
	xeCtx.MaxLoadDepth = 5
	xeCtx.GlobalSize = int(prog.GlobalSize())
	xeCtx.VTable = make(VTable)
	xeCtx.Debug = debug
	xeCtx.OPSep = "\x9B"
	xeCtx.UOPSep = "\x9C"

	xeCtx.OPSep = "|"
	xeCtx.UOPSep = ":"

	var err error

	for _, str := range prog.Strings() {
		xeCtx.Strings = append(xeCtx.Strings, string(str))
	}

	for _, f := range prog.VarInitFunctions() {
		xeCtx.VarInitFuncs = append(xeCtx.VarInitFuncs, int(f.Addr()))
	}

	for _, f := range prog.InitFunctions() {
		xeCtx.InitFuncs = append(xeCtx.InitFuncs, int(f.Addr()))
	}

	for _, f := range prog.UpdateFunctions() {
		xeCtx.UpdateFuncs = append(xeCtx.UpdateFuncs, int(f.Addr()))
	}

	for i, typ := range prog.Types() {
		xeCtx.VTable[i] = make(map[string]int)
		xeCtx.VTable[i]["#size"] = int(typ.Size())
		switch typ := typ.(type) {
		case *compiler.DerivedType:
			for _, method := range typ.Methods(false) {
				methodFunc := typ.Method(method.Name, false)
				xeCtx.VTable[i][method.Name] = int(methodFunc.InfoAddr())
			}
		case *compiler.PointerType:
			switch typ := typ.Pointee().(type) {
			case *compiler.DerivedType:
				for _, method := range typ.Methods(true) {
					methodFunc := typ.Method(method.Name, true)
					xeCtx.VTable[i][method.Name] = int(methodFunc.InfoAddr())
				}
			}
		}
	}

	xeCtx.Code = make(map[PageAddr]string)

	if debug {
		log.Printf("Program BC:%d PageSize:%d", len(prog.Bytecode()), xeCtx.PageSize)
	}

	for _, extern := range prog.ExternFuncs() {
		ftype := extern.Type().(*compiler.FunctionType)
		argTypes := make([]ArgWrapper, 0)
		var size Size
		size += ftype.Return.Size()
		for _, param := range ftype.Parameters {
			var wrapper ArgWrapper
			if param.Kind() == compiler.KindString {
				wrapper.Prefix = "@aeon_str_load("
				wrapper.Suffix = ")"
			}

			size += param.Size()

			argTypes = append(argTypes, wrapper)
		}
		xeCtx.ExternFuncs = append(xeCtx.ExternFuncs, ExternFuncEntry{
			ArgTypes:  argTypes,
			HasReturn: ftype.Return.Kind() != compiler.KindVoid,
			Size:      int(size),
			Name:      extern.Name(),
		})
	}

	for i, bc := range prog.Bytecode() {
		var buf bytes.Buffer
		err := xeCtx.marshalByteCode(&buf, bc)
		if err != nil {
			return err
		}

		bcBytes := buf.Bytes()

		page := i / xeCtx.PageSize
		pageAddr := i % xeCtx.PageSize

		if debug {
			log.Printf("%d:%d:%s", page, pageAddr, bc)
		}
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

func (x *xenonContext) marshalByteCode(w io.Writer, bc compiler.Bytecode) error {
	fmt.Fprintf(w, "%s%s", bc.Name(), x.OPSep)

	switch bc := bc.(type) {
	case compiler.Nop:
	case compiler.Mov:
		err := x.marshalOperand(w, bc.Dst)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s", x.OPSep)
		err = x.marshalOperand(w, bc.Src)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s%d", x.OPSep, int(bc.Size))
	case compiler.Alc:
		err := x.marshalOperand(w, bc.Dst)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s", x.OPSep)
		err = x.marshalOperand(w, bc.Size)
		if err != nil {
			return err
		}
	case compiler.App:
		err := x.marshalOperand(w, bc.Dst)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s", x.OPSep)
		err = x.marshalOperand(w, bc.Src)
		if err != nil {
			return err
		}
		fmt.Fprintf(w, "%s", x.OPSep)
		err = x.marshalOperand(w, bc.Elem)
		if err != nil {
			return err
		}
		fmt.Fprintf(w, "%s%d", x.OPSep, int(bc.Size))
	case compiler.BinOp:
		err := x.marshalOperand(w, bc.Dst)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s%s", x.OPSep, bc.Op)

		fmt.Fprintf(w, "%s", x.OPSep)
		err = x.marshalOperand(w, bc.Left)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s", x.OPSep)
		err = x.marshalOperand(w, bc.Right)
		if err != nil {
			return err
		}
	case compiler.UnOp:
		err := x.marshalOperand(w, bc.Dst)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s%s", x.OPSep, bc.Op)

		fmt.Fprintf(w, "%s", x.OPSep)
		err = x.marshalOperand(w, bc.Src)
		if err != nil {
			return err
		}
	case compiler.Jmp:
		err := x.marshalOperand(w, bc.Cond)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s", x.OPSep)
		err = x.marshalOperand(w, bc.Target)
		if err != nil {
			return err
		}
	case compiler.Ret:
		fmt.Fprintf(w, "%d", int(bc.Args))
	case compiler.Str:
		err := x.marshalOperand(w, bc.Dst)
		if err != nil {
			return err
		}
		fmt.Fprintf(w, "%s%s", x.OPSep, string(bc.Str))
	case compiler.Cal:
		err := x.marshalOperand(w, bc.Func)
		if err != nil {
			return err
		}
	}

	return nil
}

const bytecodeSep = "|"

const bytecodeOpStackSep = "/"

func (x *xenonContext) marshalOperand(w io.Writer, op *compiler.Operand) error {
	switch op.Kind {
	case compiler.OperandKindImmediate:
		switch imm := op.Value.(type) {
		case Int:
			fmt.Fprintf(w, "I%v", int(imm))
		case Float:
			fmt.Fprintf(w, "I%v", float64(imm))
		case Bool:
			if imm {
				fmt.Fprintf(w, "I1")
			} else {
				fmt.Fprintf(w, "I0")
			}
		case String:
			panic("BAD")
		}

		return nil
	case compiler.OperandKindRegister:
		fmt.Fprintf(w, "R%d", int(op.Value.(Register)))

		return nil
	case compiler.OperandKindIndirect:
		err := x.marshalOperand(w, op.Value.(compiler.Indirect).Ptr)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s%s", x.UOPSep, op.Kind.String())

		return nil
	case compiler.OperandKindUnary:
		err := x.marshalOperand(w, op.Value.(compiler.UnaryOperand).A)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s%s", x.UOPSep, op.Value.(compiler.UnaryOperand).Op)

		return nil
	case compiler.OperandKindBinary:
		err := x.marshalOperand(w, op.Value.(compiler.BinaryOperand).Left)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s", x.UOPSep)
		err = x.marshalOperand(w, op.Value.(compiler.BinaryOperand).Right)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s%s", x.UOPSep, op.Value.(compiler.BinaryOperand).Op)

		return nil
	case compiler.OperandKindVTableLookup:
		err := x.marshalOperand(w, op.Value.(compiler.VTableLookup).Type)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s", x.UOPSep)
		err = x.marshalOperand(w, op.Value.(compiler.VTableLookup).Method)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s%s", x.UOPSep, op.Kind.String())

		return nil
	default:
		return fmt.Errorf("unknown operand kind %v", op.Kind)
	}
}
