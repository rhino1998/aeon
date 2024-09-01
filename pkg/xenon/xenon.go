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
	"github.com/rhino1998/aeon/pkg/compiler/air"
	"github.com/rhino1998/aeon/pkg/compiler/kinds"
	"github.com/rhino1998/aeon/pkg/compiler/operators"
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

	for i, bc := range prog.Instructions() {
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

func (x *xenonContext) marshalByteCode(w io.Writer, bc air.Instruction) error {
	fmt.Fprintf(w, "%s%s", bc.Name(), x.OPSep)

	switch bc := bc.(type) {
	case air.Nop:
	case air.Mov:
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
	case air.Alc:
		err := x.marshalOperand(w, bc.Dst)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s", x.OPSep)
		err = x.marshalOperand(w, bc.Size)
		if err != nil {
			return err
		}
	case air.App:
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
	case air.BinOp:
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
	case air.UnOp:
		err := x.marshalOperand(w, bc.Dst)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s%d", x.OPSep, int(bc.Kind))
		fmt.Fprintf(w, "%s%s", x.OPSep, bc.Op)

		fmt.Fprintf(w, "%s", x.OPSep)
		err = x.marshalOperand(w, bc.Src)
		if err != nil {
			return err
		}
	case air.Jmp:
		err := x.marshalOperand(w, bc.Cond)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s", x.OPSep)
		err = x.marshalOperand(w, bc.Target)
		if err != nil {
			return err
		}
	case air.Ret:
		fmt.Fprintf(w, "%d", int(bc.Args))
	case air.Cal:
		err := x.marshalOperand(w, bc.Func)
		if err != nil {
			return err
		}
	}

	return nil
}

func (x *xenonContext) marshalOperand(w io.Writer, op *air.Operand) error {
	switch op.Kind {
	case air.OperandKindImmediate:
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
		}

		return nil
	case air.OperandKindRegister:
		fmt.Fprintf(w, "I%dIR", int(op.Value.(Register)))

		return nil
	case air.OperandKindIndirect:
		err := x.marshalOperand(w, op.Value.(air.Indirect).Ptr)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s", op.Kind.String())

		return nil
	case air.OperandKindUnary:
		err := x.marshalOperand(w, op.Value.(air.UnaryOperand).A)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s", operatorChar(op.Value.(air.UnaryOperand).Op))

		return nil
	case air.OperandKindBinary:
		err := x.marshalOperand(w, op.Value.(air.BinaryOperand).Left)
		if err != nil {
			return err
		}

		err = x.marshalOperand(w, op.Value.(air.BinaryOperand).Right)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s", operatorChar(op.Value.(air.BinaryOperand).Op))

		return nil
	case air.OperandKindVTableLookup:
		err := x.marshalOperand(w, op.Value.(air.VTableLookup).Type)
		if err != nil {
			return err
		}

		err = x.marshalOperand(w, op.Value.(air.VTableLookup).Method)
		if err != nil {
			return err
		}

		fmt.Fprintf(w, "%s", op.Kind.String())

		return nil
	default:
		return fmt.Errorf("unknown operand kind %v", op.Kind)
	}
}

func operatorChar(op operators.Operator) string {
	switch op {
	case operators.Addition,
		operators.Subtraction,
		operators.Multiplication,
		operators.Division,
		operators.LessThan,
		operators.GreaterThan,
		operators.Not,
		operators.BoundsCheck,
		operators.Modulo:
		return string(op)
	case operators.Equal:
		return "="
	case operators.NotEqual:
		return "≠"
	case operators.LessThanOrEqual:
		return "󰥽"
	case operators.GreaterThanOrEqual:
		return "≥"
	default:
		return "?"
	}
}
