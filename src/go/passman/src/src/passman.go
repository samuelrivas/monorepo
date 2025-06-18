package main

import (
	"fmt"
	"io"
	"os"
	"path/filepath"

	"filippo.io/age"
	"golang.org/x/term"
)

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

func askForPassword() string {
	fmt.Fprint(os.Stderr, "Enter password: ")
	password, err := term.ReadPassword(int(os.Stdin.Fd()))
	if err != nil {
		logLn("Error reading password")
		panic(err)
	}
	fmt.Fprintln(os.Stderr)
	return string(password)
}

func main() {
	args := os.Args
	fileName, err := parseArgs(args)
	if err != nil {
		usage(args)
		panic(err)
	}

	fd, err := os.Open(fileName)
	if err != nil {
		logLn("Error opening file")
		panic(err)
	}
	defer fd.Close()

	password:= askForPassword()

	identity, err := age.NewScryptIdentity(string(password))
	if err != nil {
		logLn("Error creating identity")
		panic(err)
	}

	cleartext, err := age.Decrypt(fd, identity)
	if err != nil {
		logLn("Error decrypting file")
		panic(err)
	}

	io.Copy(os.Stdout, cleartext)
}
