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

	// This is archived in favour of go's inferior "errors" package. If we
	// plan to implement more things here, consider implementing your own
	// errors with stack traces instead of depending on frozen external
	// package
	"github.com/pkg/errors"
	"strings"
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

func errorAndExit(message string, args ...any) {
	errorMessageLn(message, args...)
	os.Exit(1)
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
	case "update":
		return update
	default:
		panic("Unknown subcommand " + arg)
	}
}

func openFileRead(filename string) *os.File {
	slog.Info("Opening to read", "filename", filename)
	fd, err := os.Open(filename)
	if err != nil {
		errorMessageLn("Error opening %s for read", filename)
		panic(err)
	}
	return fd
}

func openFileWrite(filename string) *os.File {
	slog.Info("Opening to write", "filename", filename)
	fd, err := os.OpenFile(filename, os.O_WRONLY|os.O_TRUNC, 0)
	if err != nil {
		errorMessageLn("Error opening %s for write", filename)
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
	slog.Info("Creating identity")
	if err != nil {
		errorMessageLn("Error creating identity")
		panic(err)
	}

	slog.Info("Decrypting", "file", fd.Name())
	clearReader, err := age.Decrypt(fd, identity)
	if err != nil {
		errorMessageLn("Error decrypting %s", filename)
		panic(err)
	}
	slog.Info("Decrypted", "file", fd.Name())

	cleartext, err := io.ReadAll(clearReader)
	if err != nil {
		errorMessageLn("Error reading decrypted bytes")
		panic(err)
	}
	return string(cleartext), password
}

func writeToFile(filename string, fd *os.File) {
	destFd := openFileWrite(filename)
	defer destFd.Close()

	_, err := io.Copy(destFd, fd)
	if err != nil {
		errorMessageLn("Error copying %s to %s", fd.Name(), filename)
		panic(err)
	}
}

// Encrypts into a temp file, copies the file onto filename, and then cleans the the temp file
func encrypt(clearText, password, filename string) {
	recipient, err := age.NewScryptRecipient(password)
	if err != nil {
		errorMessageLn("Error creating recipient")
		panic(err)
	}

	tmpFd, err := os.CreateTemp("", "passman-*.age")
	if err != nil {
		errorMessageLn("Error creating temporary file")
		panic(err)
	}
	defer os.Remove(tmpFd.Name())
	defer tmpFd.Close()
	slog.Info("Created temporary file", "tmp", tmpFd.Name())

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
	sensitive bool) (bytes.Buffer, bytes.Buffer, int) {

	showableArgs := args
	if sensitive {
		showableArgs = []string{"<redacted>"}
	}
	slog.Info("Executing command", "command", command, "args", showableArgs)
	cmd := exec.Command(command, args...)

	cmd.Stdin = bytes.NewBufferString(input)

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	err := cmd.Run()

	exitCode := 0
	if err != nil {
		// Execution can fail for multiple reasons. We want to abort in
		// most cases, but if executiona failed because the command
		// exited with non-zero status, we want to propagate the exit
		// code instead
		var exitError *exec.ExitError
		if errors.As(err, &exitError) {
			exitCode = exitError.ExitCode()
		} else {
			errorMessageLn("Could not execture %s", command)
			panic(err)
		}
	}
	return stdout, stderr, exitCode
}

func runSexp(
	command, query, document string,
	sensitive bool) (string, string, int) {
	stdout, stderr, status := runCommand(
		"sexp", []string{command, query}, document, sensitive)

	return stdout.String(), stderr.String(), status
}

func runSexpQuery(query, document string, sensitive bool) string {
	stdout, stderr, status := runSexp("query", query, document, sensitive)
	// If the query returns nothing, sexp exits with code 1, though it is
	// not really an error. When an actual error happens it also exits with
	// 1, but prints a message. Since we don't care about recovery, we panic
	// in the latter case, but we need to return in the former
	if status != 0 && len(stderr) > 0 {
		errorMessageLn(
			"Error running query:\n"+
				"vvvvvvvvv stderr vvvvvvvvv\n\n"+
				"%s\n"+
				"^^^^^^^^^ stderr ^^^^^^^^^\n",
			stderr)
		panic(fmt.Errorf("sexp exited with non-zero status: %d", status))
	} else if status != 0 {
		slog.Info("query failed without error output, assuming no results")
	}

	return stdout
}

func validateExitStatus(what string, status int) {
	if status != 0 {
		panic(fmt.Errorf(
			"%s exited with non-zero status %d", what, status))
	}
}

func runSexpChange(query, document string, sensitive bool) string {
	output, _, status := runSexp("change", query, document, sensitive)
	validateExitStatus("sexp change", status)
	return output
}

func toSplice(x map[string]string) string {
	out := ""

	for k, v := range x {
		out += fmt.Sprintf("(%s \"%s\")", k, v)
	}
	return out
}

func toSexp(x map[string]string) string {
	out := "("

	for k, v := range x {
		out += fmt.Sprintf("(%s \"%s\")", k, v)
	}
	return fmt.Sprintf("(%s)", toSplice(x))
}

func query(parsedArgs ParsedArgs) {
	slog.Info("Running query subcommand", "args", parsedArgs.tailArgs)

	if len(parsedArgs.tailArgs) != 1 {
		errorMessageLn("query requires an argument with the query")
		panic("Invalid arguments")
	}

	cleartext, _ := getCleartext(parsedArgs.filename)
	output := runSexpQuery(parsedArgs.tailArgs[0], cleartext, false)

	fmt.Print(output)
}

func toMap(x []string) map[string]string {
	if len(x)%2 != 0 {
		errorMessageLn(
			"Attempting to convert map of odd length %d to object",
			len(x))
		panic("Bad conversion to pairs")
	}
	out := map[string]string{}
	for i := 0; i < len(x)-1; i += 2 {
		out[x[i]] = x[i+1]
	}
	return out
}

func get(parsedArgs ParsedArgs) {
	slog.Info("Running get subcommand", "args", parsedArgs.tailArgs)

	if len(parsedArgs.tailArgs) < 1 {
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

func validateAddFields(object map[string]string) {
	password := false
	site := false

	for k := range object {
		if k == "password" {
			password = true
		}
		if k == "site" {
			site = true
		}
	}
	if !(password && site) {
		errorMessageLn("Either password or site are not present")
		panic("Invalid add arguments")
	}
}

func add(parsedArgs ParsedArgs) {
	slog.Info("Running add subcommand", "args", parsedArgs.tailArgs)

	fields := toMap(parsedArgs.tailArgs)
	validateAddFields(fields)

	cleartext, password := getCleartext(parsedArgs.filename)

	if siteExists(cleartext, fields["site"]) {
		errorAndExit("Site %s already exists", fields["site"])
	}

	query := fmt.Sprintf("(rewrite (@x) (@x %s))", toSexp(fields))
	output := runSexpChange(query, cleartext, true)

	encrypt(output, password, parsedArgs.filename)

	fmt.Println("Added entry successfully")
}

func update(parsedArgs ParsedArgs) {
	slog.Info("Running update subcommand", "args", parsedArgs.tailArgs)

	if len(parsedArgs.tailArgs) < 3 {
		errorAndExit("Update reqires at least <site> <field> <value> as arguments")
	}

	cleartext, _ := getCleartext(parsedArgs.filename)

	fields := toMap(parsedArgs.tailArgs[1:])
	site := parsedArgs.tailArgs[0]

	if !siteExists(cleartext, site) {
		errorAndExit("Site %s doesn't exist", site)
	}

	existingFieldNames := getSiteFieldNames(cleartext, site)
	existingFields := map[string]string{}
	for k, v := range fields {
		if member(k, existingFieldNames) {
			existingFields[k] = v
		}
	}

	// The query we use for updating fields fails if there are new fields in
	// it so we first update the existing fields and then add the new ones
	output := updateExistingFields(cleartext, site, existingFields, false)

	// TODO add the new fields
	fmt.Println("Updated entry successfully: ", output)
}

// Update fields of a site, the site must exists and have the fields already, we
// guarantee this before calling this function
func updateExistingFields(
	cleartext,
	site string,
	fields map[string]string,
	sensitive bool) string {

	fieldLhs := make(map[string]string)
	for k := range fields {
		fieldLhs[k] = "$" + k
	}

	changeMatch := toSplice(fieldLhs)
	changeValue := toSplice(fields)
	query := fmt.Sprintf(
		"(children (seq (try (rewrite_record ((site %s) %s @tail) ((site %s) %s @tail)))))",
		site, changeMatch, site, changeValue)

	return runSexpChange(query, cleartext, sensitive)
}

func siteExists(cleartext, site string) bool {
	query := fmt.Sprintf("each (field site) (equals \"%s\")", site)
	output := runSexpQuery(query, cleartext, false)
	slog.Info("output", "output", output)
	return len(output) != 0
}

// return a "set" with all the field names of a given site
func getSiteFieldNames(cleartext, site string) map[string]struct{} {
	query := fmt.Sprintf(
		"each (test (field site) (equals \"%s\")) each (index 0)",
		site)
	output := runSexpQuery(query, cleartext, false)

	fields := make(map[string]struct{})
	for v := range strings.SplitSeq(output, "\n") {
		fields[v]= struct{}{}
	}
	return fields
}

func member(key string, set map[string]struct{}) bool {
	_, ok := set[key]
	return ok
}

func main() {
	args := os.Args
	parsedArgs, err := parseArgs(args)
	if err != nil {
		errorMessageLn("Error parsing arguments: %s", err)
		usage(args)
		os.Exit(1)
	}

	parsedArgs.subcommand(parsedArgs)
}
