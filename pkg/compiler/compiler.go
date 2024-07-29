package compiler

import (
	"context"
	"fmt"
	"io/fs"
	"log"
	"log/slog"

	"github.com/rhino1998/aeonvm/pkg/compiler/parser"
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

func (c *Compiler) Compile(ctx context.Context) error {
	for _, file := range c.Config.Files {
		f, err := c.Config.Src.Open(file)
		if err != nil {
			return fmt.Errorf("failed to open file %q: %w", file, err)
		}
		ast, err := parser.ParseReader(file, f)
		if err != nil {
			return fmt.Errorf("failed to parse file %q: %w", file, err)
		}

		log.Printf("%#v", ast)
	}

	return nil
}
