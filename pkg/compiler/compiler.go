package compiler

import (
	"context"
	"fmt"
	"io/fs"
	"log/slog"
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
	return nil
}
