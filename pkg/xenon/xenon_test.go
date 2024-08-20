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

func TestRuntime(t *testing.T) {
	ctx := context.Background()
	t.Parallel()

	dir := os.DirFS("./testdata/")
	testFiles, err := fs.Glob(dir, "*.txt")
	if err != nil {
		t.Fatal(err)
	}

	// TODO: wrap xenoncode compiler
	return

	for _, testFile := range testFiles {
		t.Run(testFile, func(t *testing.T) {
			r := require.New(t)
			logger := slogt.New(t)
			c, err := compiler.New(logger, compiler.Config{})
			r.NoError(err)

			testData, err := fs.ReadFile(dir, testFile)
			r.NoError(err)

			parts := bytes.SplitN(testData, []byte("\n---\n"), 2)
			source := bytes.TrimSpace(parts[0])
			expected := bytes.TrimSpace(parts[1])

			var output bytes.Buffer

			externs := xenon.DefaultExternFuncs()
			externs["print"] = xenon.RuntimeExternFuncEntry{
				ArgSize:    1,
				ReturnSize: 0,
				Func: func(r *xenon.Runtime, s []float64) float64 {
					str, err := r.LoadString(xenon.Addr(s[0]))
					if err != nil {
						panic(err)
					}
					fmt.Fprintln(&output, string(str))
					return 0
				},
			}

			c.AddFile(testFile, bytes.NewReader(source))

			prog, err := c.Compile(ctx)
			r.NoError(err)

			runtime, err := xenon.NewRuntime(prog, externs, MemPages, Registers, false)
			r.NoError(err)

			err = runtime.Run(ctx)
			r.NoError(err)

			result := output.Bytes()
			result = bytes.TrimSpace(result)
			r.Equal(expected, result)
		})
	}
}
