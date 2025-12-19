package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"gots/lang"
	"gots/lsp"
	"gots/translator"
)

func main() {
	if len(os.Args) < 2 {
		usage()
		return
	}

	switch os.Args[1] {
	case "translate":
		runTranslate(os.Args[2:])
	case "run":
		runRun(os.Args[2:])
	case "lsp":
		server := lsp.NewServer(os.Stdin, os.Stdout)
		if err := server.Run(); err != nil {
			fmt.Fprintf(os.Stderr, "lsp server error: %v\n", err)
			os.Exit(1)
		}
	default:
		usage()
	}
}

func runTranslate(args []string) {
	var outPath string
	var inputs []string

	for i := 0; i < len(args); i++ {
		arg := args[i]
		switch {
		case arg == "-h" || arg == "--help":
			usage()
			return
		case arg == "-o":
			if i+1 >= len(args) {
				fmt.Fprintln(os.Stderr, "-o flag requires a path")
				os.Exit(1)
			}
			outPath = args[i+1]
			i++
		case strings.HasPrefix(arg, "-o="):
			outPath = strings.TrimPrefix(arg, "-o=")
		default:
			inputs = append(inputs, arg)
		}
	}

	if len(inputs) < 1 {
		fmt.Fprintln(os.Stderr, "translate requires a .gots input file")
		os.Exit(1)
	}

	inPath := inputs[0]
	if outPath == "" {
		outPath = strings.TrimSuffix(inPath, filepath.Ext(inPath)) + ".go"
	}
	if err := translateToFile(inPath, outPath); err != nil {
		fmt.Fprintf(os.Stderr, "failed to translate %s: %v\n", inPath, err)
		os.Exit(1)
	}
	fmt.Printf("wrote %s\n", outPath)
}

func runRun(args []string) {
	if len(args) < 1 {
		fmt.Fprintln(os.Stderr, "run requires a .gots input file")
		os.Exit(1)
	}
	inPath := args[0]
	tmpDir, err := os.MkdirTemp("", "gots-run-*")
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to create temp dir: %v\n", err)
		os.Exit(1)
	}
	outPath := filepath.Join(tmpDir, "main.go")
	if err := translateToFile(inPath, outPath); err != nil {
		fmt.Fprintf(os.Stderr, "failed to translate %s: %v\n", inPath, err)
		os.Exit(1)
	}
	cmd := exec.Command("go", "run", outPath)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		os.Exit(1)
	}
}

func translateToFile(inPath, outPath string) error {
	data, err := os.ReadFile(inPath)
	if err != nil {
		return fmt.Errorf("failed to read input file: %w", err)
	}
	prog, diags := lang.Parse(string(data))
	if len(diags) > 0 {
		return fmt.Errorf("cannot translate; found %d issue(s)", len(diags))
	}
	code, tdiags := translator.Translate(prog)
	if len(tdiags) > 0 {
		return fmt.Errorf("cannot translate; found %d issue(s)", len(tdiags))
	}
	if err := os.MkdirAll(filepath.Dir(outPath), 0755); err != nil {
		return fmt.Errorf("failed to create output dir: %w", err)
	}
	return os.WriteFile(outPath, []byte(code), 0644)
}

func usage() {
	fmt.Println("Usage:")
	fmt.Println("  gots translate <file.gots> [-o out.go]")
	fmt.Println("  gots run <file.gots>")
	fmt.Println("  gots lsp (Language Server)")
}
