package main

import (
	"fmt"
	"os"
	"path/filepath"
)

type Abser interface {
	Abs() float64
}


func parseArgs(args []string) (string, error) {
	if len(args) < 2 {
		return "", fmt.Errorf("Missing mandatory arguments")
	}
	return args[1], nil
}

func log(message string, args ... any) {
	if len(args) > 0 {
		message = fmt.Sprintf(message, args...)
	}
	fmt.Fprintf(os.Stderr, "%s", message)
}

func logLn(message string, args ... any) {
	log(message + "\n", args...)
}

func usage(args []string) {
	log("Usage: %s <filename>\n\n", filepath.Base(args[0]))
}

func main() {
	args := os.Args
	fileName, err := parseArgs(args)
	if err != nil {
		usage(args)
		logLn("Error: %v", err)
		return
	}
	content, err := os.ReadFile(fileName)
	if err != nil {
		logLn("Error reading file: %v", err)
		return
	}
	fmt.Println("File content:", string(content))
}
