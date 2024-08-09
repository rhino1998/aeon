package main

import (
	"context"
	"fmt"
	"log"
	"log/slog"
	"os"
	"os/signal"
	"path/filepath"
	"syscall"

	"github.com/rhino1998/aeon/pkg/compiler"
	"github.com/rhino1998/aeon/pkg/interpreter"
	"github.com/rhino1998/aeon/pkg/xenon"
	"github.com/urfave/cli/v3"
)

func main() {
	ctx, cancel := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer cancel()

	cmd := &cli.Command{
		Name:  "aeon",
		Usage: "The Aeon compiler",
		Commands: []*cli.Command{
			{
				Name:  "build",
				Usage: "Compile Aeon source code into bytecode and a XenonCode VM",
				Action: func(ctx context.Context, c *cli.Command) error {
					if c.Args().Len() != 1 {
						return fmt.Errorf("must provide at least one aeon file or directory as argument")
					}

					path := c.Args().First()
					stat, err := os.Stat(path)
					if err != nil {
						return fmt.Errorf("invalid path: %w", err)
					}

					logger := slog.Default()

					config := compiler.Config{}

					compiler, err := compiler.New(logger, config)
					if err != nil {
						return fmt.Errorf("failed to initialize compiler: %w", err)
					}

					if stat.IsDir() {
						files, err := filepath.Glob(filepath.Join(path, "*.ae"))
						if err != nil {
							return fmt.Errorf("failed to find aeon files in directory: %w", err)
						}

						for _, file := range files {
							f, err := os.Open(file)
							if err != nil {
								return fmt.Errorf("failed to open file: %w", err)
							}

							compiler.AddFile(file, f)
						}
					} else {
						f, err := os.Open(path)
						if err != nil {
							return fmt.Errorf("failed to open file: %w", err)
						}

						compiler.AddFile(path, f)
					}

					prog, err := compiler.Compile(ctx)
					if err != nil {
						return err
					}

					out, err := os.Create("main.xc")
					if err != nil {
						return err
					}
					defer out.Close()

					err = xenon.EmitXenonCode(out, prog)
					if err != nil {
						return err
					}

					return nil
				},
			},
			{
				Name:  "interpet",
				Usage: "Interpret and run Aeon source code",
				Action: func(ctx context.Context, c *cli.Command) error {
					if c.Args().Len() != 1 {
						return fmt.Errorf("must provide at least one aeon file or directory as argument")
					}

					path := c.Args().First()
					stat, err := os.Stat(path)
					if err != nil {
						return fmt.Errorf("invalid path: %w", err)
					}

					logger := slog.Default()

					config := compiler.Config{}

					compiler, err := compiler.New(logger, config)
					if err != nil {
						return fmt.Errorf("failed to initialize compiler: %w", err)
					}

					if stat.IsDir() {
						files, err := filepath.Glob(filepath.Join(path, "*.ae"))
						if err != nil {
							return fmt.Errorf("failed to find aeon files in directory: %w", err)
						}

						for _, file := range files {
							f, err := os.Open(file)
							if err != nil {
								return fmt.Errorf("failed to open file: %w", err)
							}

							compiler.AddFile(file, f)
						}
					} else {
						f, err := os.Open(path)
						if err != nil {
							return fmt.Errorf("failed to open file: %w", err)
						}

						compiler.AddFile(path, f)
					}

					prog, err := compiler.Compile(ctx)
					if err != nil {
						fmt.Fprintf(os.Stderr, "%v", err)
						os.Exit(1)
					}

					return interpreter.Execute(prog, "main.main")
				},
			},
			{
				Name:  "run",
				Usage: "Generate Aeon bytecode and execute it",
				Flags: []cli.Flag{
					&cli.BoolFlag{
						Name:    "debug",
						Aliases: []string{"d"},
					},
				},
				Action: func(ctx context.Context, c *cli.Command) error {
					if c.Args().Len() != 1 {
						return fmt.Errorf("must provide at least one aeon file or directory as argument")
					}

					path := c.Args().First()
					stat, err := os.Stat(path)
					if err != nil {
						return fmt.Errorf("invalid path: %w", err)
					}

					logger := slog.Default()

					config := compiler.Config{}

					compiler, err := compiler.New(logger, config)
					if err != nil {
						return fmt.Errorf("failed to initialize compiler: %w", err)
					}

					if stat.IsDir() {
						files, err := filepath.Glob(filepath.Join(path, "*.ae"))
						if err != nil {
							return fmt.Errorf("failed to find aeon files in directory: %w", err)
						}

						for _, file := range files {
							f, err := os.Open(file)
							if err != nil {
								return fmt.Errorf("failed to open file: %w", err)
							}

							compiler.AddFile(file, f)
						}
					} else {
						f, err := os.Open(path)
						if err != nil {
							return fmt.Errorf("failed to open file: %w", err)
						}

						compiler.AddFile(path, f)
					}

					prog, err := compiler.Compile(ctx)
					if err != nil {
						fmt.Fprintf(os.Stderr, "%v", err)
						os.Exit(1)
					}

					runtime, err := xenon.NewRuntime(prog, xenon.DefaultExternFuncs(), 5, 16, c.Bool("debug"))
					if err != nil {
						return err
					}

					err = runtime.Run(ctx, "main.varinit")
					if err != nil {
						return err
					}

					err = runtime.Run(ctx, "main.init")
					if err != nil {
						return err
					}

					err = runtime.Run(ctx, "main.main")
					if err != nil {
						return err
					}

					return nil
				},
			},
		},
	}

	err := cmd.Run(ctx, os.Args)
	if err != nil {
		log.Fatalln(err)
	}
}
