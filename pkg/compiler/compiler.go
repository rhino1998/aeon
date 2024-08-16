package compiler

import (
	"context"
	"fmt"
	"io"
	"log/slog"
	"path/filepath"

	"github.com/rhino1998/aeon/pkg/parser"
)

type Config struct {
}

func (c *Config) Validate(logger *slog.Logger) error {
	return nil
}

type fileEntry struct {
	name string
	f    io.Reader
}

type Compiler struct {
	logger *slog.Logger
	Config Config

	files []fileEntry
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
	for _, file := range c.files {
		ast, err := parser.ParseReader(file.name, file.f, parser.InitState("filename", file.name))
		if err != nil {
			errs.Add(err)
			continue
		}

		err = c.compileFile(prog, file.name, ast.(parser.File))
		if err != nil {
			errs.Add(err)
			continue
		}
	}

	err = c.resolveProgramTypes(prog)
	if err != nil {
		errs.Add(err)
	}

	err = prog.compileBytecode(ctx)
	if err != nil {
		errs.Add(err)
	}

	return prog, nil
}

func (c *Compiler) AddFile(name string, f io.Reader) {
	c.files = append(c.files, fileEntry{
		name: filepath.Base(name),
		f:    f,
	})
}
