package main

import (
	"fmt"
	"log/slog"
	"os"
	"path/filepath"
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

func usage(args []string) {
	errorMessage(`Usage: %s <filename> <subcommand>

  - query <query>

        Runs a sexp query on the decrypted file

  - get <regex> [<field> ...]

        Gets the fields of all sites that match regex. You typically would use
        something like "site password" as fields unless you know exactly which
        fields you're matching. It returns the whole object if no field is
        specified.

  - add <site> <value_1> <field_2> <value_2>...

        Adds a new site with the site name and an arbitrary number of additional fields.

        A value of the form XXX triggers a prompt to input it via tty. This is
        useful to prevent leaking secrets in the shell history.

  - update <site> <field_1> <value_1> <field_2> <value_2>...

        Updates the fields of an existing site. The site must exist. Existing
        fields are updated, new fields are added.

        A value XXX triggers the same functionality as for the add command.
`,
		filepath.Base(args[0]))
}

func setupLogger(level slog.Level) {
	loggerOptions := slog.HandlerOptions{
		Level: level,
		ReplaceAttr: func(groups []string, a slog.Attr) slog.Attr {
			if a.Key == "time" || a.Key == "level" {
				return slog.Attr{}
			}
			return a
		},
	}
	logger := slog.New(slog.NewTextHandler(os.Stderr, &loggerOptions))
	slog.SetDefault(logger)
}

func get_log_level() slog.Level {
	defaultLevel := slog.LevelError

	level := os.Getenv("LOG_LEVEL")
	switch level {
	case "debug":
		return slog.LevelDebug
	case "info":
		return slog.LevelInfo
	case "warn":
		return slog.LevelWarn
	case "error":
		return slog.LevelError
	case "":
		return defaultLevel
	default:
		errorMessageLn(
			"LOG_LEVEL is set to an unknown log level %s", level)
		panic("Unknown log level " + level)
	}
}

func main() {
	level := get_log_level()
	setupLogger(level)
	args := os.Args
	parsedArgs, err := parseArgs(args)
	if err != nil {
		errorMessageLn("Error parsing arguments: %s", err)
		usage(args)
		os.Exit(1)
	}

	parsedArgs.subcommand(parsedArgs)
}
