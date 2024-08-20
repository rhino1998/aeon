package xenon_test

import (
	"bufio"
	"bytes"
	"context"
	"io"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/neilotoole/slogt"
	"github.com/rhino1998/aeon/pkg/compiler"
	"github.com/rhino1998/aeon/pkg/xenon"
	"github.com/stretchr/testify/require"
)

func runXenoncode(ctx context.Context, t *testing.T, dir string, w io.Writer) error {
	compile := exec.CommandContext(ctx, "xenoncode", "-compile", dir)
	err := compile.Run()
	if err != nil {
		return err
	}

	run := exec.CommandContext(ctx, "xenoncode", "-run", dir)
	out, err := run.StdoutPipe()
	if err != nil {
		return err
	}

	go func() {
		scanner := bufio.NewScanner(out)
		for scanner.Scan() {
			line := scanner.Bytes()
			if bytes.HasPrefix(line, []byte("debug: ")) {
				t.Log(string(line[7:]))
			} else {
				_, _ = w.Write(line)
				_, _ = w.Write([]byte("\n"))
			}
		}
	}()

	return run.Run()
}

func TestXenonCode(t *testing.T) {
	ctx := context.Background()
	t.Parallel()

	dir := os.DirFS("./testdata/")
	testFiles, err := fs.Glob(dir, "*.txt")
	if err != nil {
		t.Fatal(err)
	}

	for _, testFile := range testFiles {
		name := strings.Split(testFile, ".")[0]
		t.Run(name, func(t *testing.T) {
			r := require.New(t)
			logger := slogt.New(t)
			c, err := compiler.New(logger, compiler.Config{})
			r.NoError(err)

			testData, err := fs.ReadFile(dir, testFile)
			r.NoError(err)

			parts := bytes.SplitN(testData, []byte("\n---\n"), 2)
			source := bytes.TrimSpace(parts[0])
			expected := strings.TrimSpace(string(parts[1]))

			var output bytes.Buffer

			c.AddFile(testFile, bytes.NewReader(source))

			prog, err := c.Compile(ctx)
			r.NoError(err)

			tmpDir := t.TempDir()
			tmpFilename := filepath.Join(tmpDir, "main.xc")
			tmpFile, err := os.Create(tmpFilename)
			r.NoError(err)

			err = xenon.EmitXenonCode(tmpFile, prog, true)
			r.NoError(err)

			err = tmpFile.Close()
			r.NoError(err)

			err = runXenoncode(ctx, t, tmpDir, &output)
			r.NoError(err)

			result := string(output.Bytes())
			result = strings.TrimSpace(result)
			r.Equal(expected, result)
		})
	}
}
