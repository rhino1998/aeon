package compiler

import (
	"context"
	"fmt"
	"io/fs"
	"log/slog"

	"github.com/rhino1998/aeon/pkg/parser"
)

type Config struct {
	Src   fs.FS
	Files []string
}

func (c *Config) Validate(logger *slog.Logger) error {
	return nil
}

type Compiler struct {
	logger *slog.Logger
	Config Config

	typeKinds map[string]Kind
}

func New(logger *slog.Logger, config Config) (*Compiler, error) {
	err := config.Validate(logger)
	if err != nil {
		return nil, fmt.Errorf("failed to validate compiler config: %w", err)
	}

	return &Compiler{
		logger: logger,
		Config: config,
	}, nil
}

func (c *Compiler) Compile(ctx context.Context) (_ *Program, err error) {
	errs := newErrorSet()
	defer func() {
		err = errs.Defer(err)
	}()

	prog := newProgram()
	for _, file := range c.Config.Files {
		f, err := c.Config.Src.Open(file)
		if err != nil {
			errs.Add(err)
			continue
		}
		ast, err := parser.ParseReader(file, f, parser.InitState("filename", file))
		if err != nil {
			errs.Add(err)
			continue
		}

		err = c.compileFile(prog, file, ast.(parser.File))
		if err != nil {
			errs.Add(err)
			continue
		}
	}

	err = c.resolveProgramTypes(prog)
	if err != nil {
		errs.Add(err)
		return nil, errs
	}

	err = prog.compileBytecode(ctx)
	if err != nil {
		errs.Add(err)
		return nil, errs
	}

	return prog, nil
}
