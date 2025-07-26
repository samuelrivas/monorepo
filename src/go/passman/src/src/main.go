package main

import (
	"fmt"
	"os"
	"path/filepath"
	// This is archived in favour of go's inferior "errors" package. If we
	// plan to implement more things here, consider implementing your own
	// errors with stack traces instead of depending on frozen external
	// package
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
