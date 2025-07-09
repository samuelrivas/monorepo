package main

import (
	"bytes"
	"fmt"
	"io"
	"log/slog"
	"os"
	"os/exec"
	"path/filepath"

	"filippo.io/age"
	"golang.org/x/term"
)

type ParsedArgs struct {
	// unparsed arguments
	tailArgs   []string
	filename   string
	subcommand func(args ParsedArgs)
}

func parseArgs(args []string) (ParsedArgs, error) {
	if len(args) < 3 {
		return ParsedArgs{}, fmt.Errorf("Missing mandatory arguments")
	}
	return ParsedArgs{
		tailArgs:   args[3:],
		filename:   args[1],
		subcommand: parseSubcommand(args[2]),
	}, nil
}

func errorMessage(message string, args ...any) {
	if len(args) > 0 {
		message = fmt.Sprintf(message, args...)
	}
	fmt.Fprintf(os.Stderr, "%s", message)
}

func errorMessageLn(message string, args ...any) {
	errorMessage(message+"\n", args...)
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
	case "get":
		return get
	case "add":
		return add
	default:
		panic("Unknown subcommand " + arg)
	}
}

func openFileRead(filename string) *os.File {
	slog.Info("Opening file to read", "filename", filename)
	fd, err := os.Open(filename)
	if err != nil {
		errorMessageLn("Error opening file")
		panic(err)
	}
	return fd
}

func openFileWrite(filename string) *os.File {
	slog.Info("Opening file to write", "filename", filename)
	fd, err := os.OpenFile(filename, os.O_WRONLY | os.O_TRUNC, 0)
	if err != nil {
		errorMessageLn("Error opening file")
		panic(err)
	}
	return fd
}

// Decrypt filePath and return a io.Reader on the clear text. It returns
// the identity as well so that we can re-encrpyt with it
func getCleartext(filename string) (string, string) {
	fd := openFileRead(filename)
	defer fd.Close()

	password := askForPassword()

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
	return string(cleartext), password
}

func writeToFile(filename string, fd *os.File) {
	destFd := openFileWrite(filename)
	defer destFd.Close()

	_, err := io.Copy(destFd, fd)
	if err != nil {
		errorMessageLn("Error writing to file %s", filename)
		panic(err)
	}
}

// Encrypts into a temp file, copies the file onto filename, and then cleans the the temp file
func encrypt(clearText, password, filename string) {
	recipient, err := age.NewScryptRecipient(password)
	if (err != nil) {
		errorMessageLn("Error creating recipient")
		panic(err)
	}

	tmpFd, err := os.CreateTemp("", "passman-*.age")
	if err != nil {
		errorMessageLn("Error creating temporary file")
		panic(err);
	}
	defer os.Remove(tmpFd.Name())
	defer tmpFd.Close()
	slog.Info("Created temporary file for encryption", "tmp", tmpFd.Name())

	writer, err := age.Encrypt(tmpFd, recipient)
	if err != nil {
		errorMessageLn("Error creating encryptor")
		panic(err)
	}

	_, err = io.WriteString(writer, clearText)
	if err != nil {
		errorMessageLn("Error writing to encryptor")
		panic(err)
	}

	err = writer.Close()
	if err != nil {
		errorMessageLn("Error closing encryptor")
		panic(err)
	}

	_, err = tmpFd.Seek(0, io.SeekStart)
	if err != nil {
		errorMessageLn("Error seeking to start of temporary file")
		panic(err)
	}

	slog.Info(
		"Copying temp file to destination",
		"filename", filename, "tempfile", tmpFd.Name())
	writeToFile(filename, tmpFd)
}

func runCommand(
	command string,
	args []string,
	input string,
	sensitive bool) bytes.Buffer {

	showableArgs := args
	if sensitive {
		showableArgs = []string{"<redacted>"}
	}
	slog.Info("Executing command", "command", command, "args", showableArgs)
	cmd := exec.Command(command, args...)

	cmd.Stdin = bytes.NewBufferString(input)

	var stdout bytes.Buffer
	cmd.Stdout = &stdout

	err := cmd.Run()
	if err != nil {
		errorMessageLn("This command failed: ", command)
		panic(err)
	}
	return stdout
}

func runSexp(command, query, document string, sensitive bool) string {
	output := runCommand(
		"sexp", []string{command, query}, document, sensitive)

	return output.String()
}

func runSexpQuery(query, document string, sensitive bool) string {
	return runSexp("query", query, document, sensitive)
}

func runSexpChange(query, document string, sensitive bool) string {
	return runSexp("change", query, document, sensitive)
}

func toSexp(x map[string]string) string {
	out := "("

	for k, v := range x {
		out += fmt.Sprintf("(%s \"%s\")", k, v)
	}
	return out + ")"
}

func query(parsedArgs ParsedArgs) {
	slog.Info("Running query subcommand", "args", parsedArgs.tailArgs)

	if (len(parsedArgs.tailArgs) != 1) {
		errorMessageLn("query requires an argument with the query")
		panic("Invalid arguments")
	}

	cleartext, _ := getCleartext(parsedArgs.filename)
	output := runSexpQuery(parsedArgs.tailArgs[0], cleartext, false)

	fmt.Print(output)
}

func toMap(x []string) map[string]string {
	if len(x) % 2 != 0 {
		errorMessageLn(
			"Attempting to convert map of odd length %d to object",
			len(x))
		panic("Bad conversion to pairs")
	}
	out := map[string]string{}
	for i := 0; i < len(x) - 1; i += 2 {
		out[x[i]] = x[i+1]
	}
	return out
}

func get(parsedArgs ParsedArgs) {
	slog.Info("Running get subcommand", "args", parsedArgs.tailArgs)

	if (len(parsedArgs.tailArgs) < 1) {
		errorMessageLn("get requires an argument with the site regex")
		panic(fmt.Errorf("Invalid arguments"))
	}

	cleartext, _ := getCleartext(parsedArgs.filename)
	query := fmt.Sprintf(
		"each (test (field site) (regex \"%s\"))",
		parsedArgs.tailArgs[0])

	for i := 1; i < len(parsedArgs.tailArgs); i++ {
		query = fmt.Sprintf(
			"%s (field %s)",
			query, parsedArgs.tailArgs[i])
	}

	output := runSexpQuery(query, cleartext, false)
	fmt.Print(output)
}

func validateAdd(object map[string]string) {
	password := false
	site := false

	for k,_ := range(object) {
		if k == "password" {
			password = true
		}
		if k == "site" {
			site = true
		}
	}
	if ! (password && site) {
		errorMessageLn("Either password or site are not present")
		panic("Invalid add arguments")
	}
}

func add(parsedArgs ParsedArgs) {
	slog.Info("Running add subcommand", "args", parsedArgs.tailArgs)

	fields := toMap(parsedArgs.tailArgs)
	validateAdd(fields)

	cleartext, password := getCleartext(parsedArgs.filename)
	query := fmt.Sprintf("(rewrite (@x) (@x %s))", toSexp(fields))

	output := runSexpChange(query, cleartext, true)
	encrypt(output,password, parsedArgs.filename)
	fmt.Println("Added entry successfully")
}

func main() {
	slog.Info("Hello logger")
	args := os.Args
	parsedArgs, err := parseArgs(args)
	if err != nil {
		usage(args)
		panic(err)
	}

	parsedArgs.subcommand(parsedArgs)
}
