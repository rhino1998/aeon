package xenon

import (
	"bytes"
	"context"
	_ "embed"
	"fmt"
	"io"
	"log/slog"
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

	KindNil     int
	KindInt     int
	KindFloat   int
	KindBool    int
	KindString  int
	KindPointer int
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
	xeCtx.NumCodePages = (len(prog.Bytecode()) + xeCtx.PageSize) / xeCtx.PageSize
	xeCtx.NumMemPages = 10
	xeCtx.NumStrPages = 10
	xeCtx.NumRegisters = prog.Registers()
	xeCtx.MaxLoadDepth = 5
	xeCtx.GlobalSize = int(prog.GlobalSize())
	xeCtx.VTable = make(VTable)
	xeCtx.Debug = logger.Handler().Enabled(ctx, slog.LevelDebug)
	xeCtx.OPSep = "\x9B"

	xeCtx.KindNil = int(compiler.KindNil)
	xeCtx.KindInt = int(compiler.KindInt)
	xeCtx.KindFloat = int(compiler.KindFloat)
	xeCtx.KindBool = int(compiler.KindBool)
	xeCtx.KindString = int(compiler.KindString)
	xeCtx.KindPointer = int(compiler.KindPointer)

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
		xeCtx.VTable[i]["#kind"] = int(typ.Kind())
		switch typ := typ.(type) {
		case *compiler.DerivedType:
			for _, method := range typ.Methods(false) {
				methodFunc := typ.Method(method.Name, false)
				xeCtx.VTable[i][method.String()] = int(methodFunc.InfoAddr())
			}
		case *compiler.PointerType:
			switch typ := compiler.DereferenceType(typ.Pointee()).(type) {
			case *compiler.DerivedType:
				for _, method := range typ.Methods(true) {
					methodFunc := typ.Method(method.Name, true)
					xeCtx.VTable[i][method.String()] = int(methodFunc.InfoAddr())
				}
			}
		}
	}

	xeCtx.Code = make(map[PageAddr]string)

	logger.Debug("Program",
		slog.Int("page_size", xeCtx.PageSize),
		slog.Int("code_pages", xeCtx.NumCodePages),
		slog.Int("mem_pages", xeCtx.NumMemPages),
		slog.Int("str_pages", xeCtx.NumStrPages),
		slog.Int("instructions", len(prog.Bytecode())),
	)

	for _, extern := range prog.ExternFuncs() {
		ftype := extern.Type().(*compiler.FunctionType)
		argTypes := make([]ArgWrapper, 0)
		var size Size
		size += ftype.Return.Size()

		kinds, err := extern.FlatParameterKinds()
		if err != nil {
			return fmt.Errorf("failed to get flat parameter kinds: %w", err)
		}

		for _, kind := range kinds {
			var wrapper ArgWrapper
			if kind == compiler.KindString {
				wrapper.Prefix = "@aeon_str_load("
				wrapper.Suffix = ")"
			}

			size += 1

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

		logger.Debug("debug: %d:%d:%s", slog.Int("page", page), slog.Int("pageAddr", pageAddr), slog.Any("bc", bc))
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

		fmt.Fprintf(w, "%s%d", x.OPSep, int(bc.Kind))
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

func (x *xenonContext) marshalOperand(w io.Writer, op *compiler.Operand) error {
	switch op.Kind {
	case compiler.OperandKindImmediate:
		switch imm := op.Value.(type) {
		case Int:
			fmt.Fprintf(w, "I%vI", int(imm))
		case Float:
			fmt.Fprintf(w, "I%vI", float64(imm))
		case Bool:
			if imm {
				fmt.Fprintf(w, "T")
			} else {
				fmt.Fprintf(w, "F")
			}
		case String:
			panic("BAD")
		case Nil:
			fmt.Fprintf(w, "I0I")
		}

		return nil
	case compiler.OperandKindRegister:
		fmt.Fprintf(w, "I%dIR", int(op.Value.(Register)))

		return nil
	case compiler.OperandKindIndirect:
		err := x.marshalOperand(w, op.Value.(compiler.Indirect).Ptr)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s", op.Kind.String())

		return nil
	case compiler.OperandKindUnary:
		err := x.marshalOperand(w, op.Value.(compiler.UnaryOperand).A)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s", operatorChar(op.Value.(compiler.UnaryOperand).Op))

		return nil
	case compiler.OperandKindBinary:
		err := x.marshalOperand(w, op.Value.(compiler.BinaryOperand).Left)
		if err != nil {
			return err
		}

		err = x.marshalOperand(w, op.Value.(compiler.BinaryOperand).Right)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s", operatorChar(op.Value.(compiler.BinaryOperand).Op))

		return nil
	case compiler.OperandKindVTableLookup:
		err := x.marshalOperand(w, op.Value.(compiler.VTableLookup).Type)
		if err != nil {
			return err
		}

		err = x.marshalOperand(w, op.Value.(compiler.VTableLookup).Method)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s", op.Kind.String())

		return nil
	default:
		return fmt.Errorf("unknown operand kind %v", op.Kind)
	}
}

func operatorChar(op compiler.Operator) string {
	switch op {
	case compiler.OperatorAddition,
		compiler.OperatorSubtraction,
		compiler.OperatorMultiplication,
		compiler.OperatorDivision,
		compiler.OperatorLessThan,
		compiler.OperatorGreaterThan,
		compiler.OperatorNot,
		compiler.OperatorBoundsCheck,
		compiler.OperatorModulo:
		return string(op)
	case compiler.OperatorEqual:
		return "="
	case compiler.OperatorNotEqual:
		return "≠"
	case compiler.OperatorLessThanOrEqual:
		return "󰥽"
	case compiler.OperatorGreaterThanOrEqual:
		return "≥"
	default:
		return "?"
	}
}
