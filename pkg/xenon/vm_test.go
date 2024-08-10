package xenon_test

import (
	"bytes"
	"context"
	"fmt"
	"io/fs"
	"os"
	"testing"

	"github.com/neilotoole/slogt"
	"github.com/rhino1998/aeon/pkg/compiler"
	"github.com/rhino1998/aeon/pkg/xenon"
	"github.com/stretchr/testify/require"
)

const MemPages = 10
const Registers = 16

var externs = xenon.RuntimeExternFuncs{
	"print": {
		ArgSize: 1,
		Func: func(args []any) any {
			fmt.Println(string(args[0].(xenon.String)))
			return nil
		},
	},
}

func TestAll(t *testing.T) {
	ctx := context.Background()
	t.Parallel()

	dir := os.DirFS("./testdata/")
	testFiles, err := fs.Glob(dir, "*.txt")
	if err != nil {
		t.Fatal(err)
	}

	for _, testFile := range testFiles {
		t.Run(testFile, func(t *testing.T) {
			r := require.New(t)
			logger := slogt.New(t)
			c, err := compiler.New(logger, compiler.Config{})
			r.NoError(err)

			testData, err := fs.ReadFile(dir, testFile)
			r.NoError(err)

			parts := bytes.SplitN(testData, []byte("\n---\n"), 2)

			c.AddFile(testFile, bytes.NewReader(parts[0]))

			prog, err := c.Compile(ctx)
			r.NoError(err)

			runtime, err := xenon.NewRuntime(prog, externs, MemPages, Registers, false)
			r.NoError(err)

			err = runtime.Run(ctx, "main.main")
			r.NoError(err)
		})
	}
}
