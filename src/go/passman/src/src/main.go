package main

import (
	"bytes"
	"fmt"
	"io"
	"log/slog"
	"os"
	"os/exec"
	"path/filepath"

	// This is archived in favour of go's inferior "errors" package. If we
	// plan to implement more things here, consider implementing your own
	// errors with stack traces instead of depending on frozen external
	// package
	"github.com/pkg/errors"

	"github.com/samuelrivas/monorepo/passman/internal/sets"
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
	errorMessage(`Usage: %s <filename> <subcommand>

  - query <query>

        Runs a sexp query on the decrypted file

  - get <regex> [<field> ...]

        Gets the fields of all sites that match regex. You typically would use
        something like "site password" as fields unless you know exactly which
        fields you're matching. It returns the whole object if no field is
        specified.

  - add <field_1> <value_1> <field_2> <value_2>...

        Adds a new site with the given fields. Both "site" and "password"
        are mandatory. An arbitrary number of additional fields can be added.

        A value of the form XXX triggers a prompt to input it via tty. This is
        useful to prevent leaking secrets in the shell history.

  - update <site> <field_1> <value_1> <field_2> <value_2>...

        Updates the fields of an existing site. The site must exist. Existing
        fields are updated, new fields are added.

        A value XXX triggers the same functionality as for the add command.
`,
		filepath.Base(args[0]))
}

func errorAndExit(message string, args ...any) {
	errorMessageLn(message, args...)
	os.Exit(1)
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

func validateExitStatus(what string, status int) {
	if status != 0 {
		panic(fmt.Errorf(
			"%s exited with non-zero status %d", what, status))
	}
}

// Look for values equal to XXX and prompt the user for the actual value
func replaceXXXs(x map[string]string) {
	for k, v := range x {
		if v == "XXX" {
			slog.Info("Replacing XXX with user prompt", "XXX", k)
			value := askForSafeInput("value for field " + k)
			x[k] = value
		}
	}
}

// Convert a list of strings into a map, using the key, value, key, value...
// convention
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

func splitExisting(
	fields map[string]string,
	existingFieldNames map[string]struct{}) (map[string]string, map[string]string) {

	existingFields := map[string]string{}
	newFields := map[string]string{}
	for k, v := range fields {
		if sets.Member(k, existingFieldNames) {
			existingFields[k] = v
		} else {
			newFields[k] = v
		}
	}
	return existingFields, newFields
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
