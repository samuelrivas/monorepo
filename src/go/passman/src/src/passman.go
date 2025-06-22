package main

import (
	"fmt"
	"io"
	"log/slog"
	"os"
	"path/filepath"

	"filippo.io/age"
	"golang.org/x/term"
)

type ParsedArgs struct {
	// unparsed arguments
	tailArgs []string
	filename string
	subcommand func(args ParsedArgs)
}

func parseArgs(args []string) (ParsedArgs, error) {
	if len(args) < 3 {
		return ParsedArgs{}, fmt.Errorf("Missing mandatory arguments")
	}
	return ParsedArgs{
		tailArgs: args[3:],
		filename: args[1],
		subcommand: parseSubcommand(args[2]),
	}, nil
}

func errorMessage(message string, args ... any) {
	if len(args) > 0 {
		message = fmt.Sprintf(message, args...)
	}
	fmt.Fprintf(os.Stderr, "%s", message)
}

func errorMessageLn(message string, args ... any) {
	errorMessage(message + "\n", args...)
}

func usage(args []string) {
	errorMessage("Usage: %s <filename> <subcommand>\n\n",
		filepath.Base(args[0]))
}

func askForPassword() string {
	fmt.Print("Enter password: ")
	password, err := term.ReadPassword(int(os.Stdin.Fd()))
	if err != nil {
		errorMessageLn("Error reading password")
		panic(err)
	}
	fmt.Println()
	return string(password)
}

func parseSubcommand(arg string) func(parsedArgs ParsedArgs) {
	switch arg {
	case "query":
		return query
	default:
		panic("Unknown subcommand " + arg)
	}
}

func query(parsedArgs ParsedArgs) {
	slog.Info("Running query subcommand", "args", parsedArgs.tailArgs)

	cleartext := getCleartext(parsedArgs.filename)
	fmt.Print(cleartext)
}

// Decrypt filePath and return a io.Reader on the clear text
func getCleartext(filename string) string {
	slog.Info("Opening file", "filename", filename)
	fd, err := os.Open(filename)
	if err != nil {
		errorMessageLn("Error opening file")
		panic(err)
	}
	defer fd.Close()

	password:= askForPassword()

	identity, err := age.NewScryptIdentity(string(password))
	slog.Info("Creating scrypt identity for provided password")
	if err != nil {
		errorMessageLn("Error creating identity")
		panic(err)
	}

	slog.Info("Attempting to decrypt file descriptor with created identity", "fd", fd)
	clearReader, err := age.Decrypt(fd, identity)
	if err != nil {
		errorMessageLn("Error decrypting file")
		panic(err)
	}
	slog.Info("File decrypted successfully")

	cleartext, err := io.ReadAll(clearReader)
	if err != nil {
		errorMessageLn("Error reading clear bytes")
		panic(err)
	}
	return string(cleartext)
}

func main() {
	slog.Info("Hello logger")
	args := os.Args
	parsedArgs, err := parseArgs(args)
	if err != nil {
		usage(args)
		panic(err)
	}

	fd, err := os.Open(parsedArgs.filename)
	if err != nil {
		errorMessageLn("Error opening file")
		panic(err)
	}
	defer fd.Close()

	parsedArgs.subcommand(parsedArgs)
}
