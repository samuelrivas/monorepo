package main

import (
	"fmt"
	"log/slog"

	"github.com/samuelrivas/monorepo/passman/internal/sets"
)

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

func get(parsedArgs ParsedArgs) {
	slog.Info("Running get subcommand", "args", parsedArgs.tailArgs)

	if len(parsedArgs.tailArgs) < 1 {
		errorMessageLn("get requires an argument with the site regex")
		panic(fmt.Errorf("Invalid arguments"))
	}

	cleartext, _ := getCleartext(parsedArgs.filename)
	extractionQuery := fmt.Sprintf(
		"each (test (field site) (regex \"%s\"))",
		parsedArgs.tailArgs[0])

	presentationQuery := presentFieldsQuery(parsedArgs.tailArgs[1:])
	query := extractionQuery + presentationQuery

	output := runSexpQuery(query, cleartext, false)
	fmt.Print(output)
}

func add(parsedArgs ParsedArgs) {
	slog.Info("Running add subcommand", "args", parsedArgs.tailArgs)

	if len(parsedArgs.tailArgs) < 1 {
		errorMessageLn("add requires an argument with the site name")
		panic(fmt.Errorf("Invalid arguments"))
	}

	fields := toMap(parsedArgs.tailArgs[1:])
	fields["site"] = parsedArgs.tailArgs[0]

	cleartext, password := getCleartext(parsedArgs.filename)

	if siteExists(cleartext, fields["site"]) {
		errorAndExit("Site %s already exists", fields["site"])
	}

	replaceXXXs(fields)

	query := fmt.Sprintf("(rewrite (@x) (@x %s))", toSexp(fields))
	output := runSexpChange(query, cleartext, true)

	encrypt(output, password, parsedArgs.filename)

	fmt.Println("Entry added successfully")
}

func update(parsedArgs ParsedArgs) {
	slog.Info("Running update subcommand", "args", parsedArgs.tailArgs)

	if len(parsedArgs.tailArgs) < 3 {
		errorAndExit("Update reqires at least <site> <field> <value> as arguments")
	}

	cleartext, password := getCleartext(parsedArgs.filename)

	fields := toMap(parsedArgs.tailArgs[1:])
	site := parsedArgs.tailArgs[0]

	if !siteExists(cleartext, site) {
		errorAndExit("Site %s doesn't exist", site)
	}

	existingFieldNames := getSiteFieldNames(cleartext, site)

	replaceXXXs(fields)
	existingFields, newFields := splitExisting(fields, existingFieldNames)

	output := updateFields(cleartext, site, existingFields, newFields, true)
	encrypt(output, password, parsedArgs.filename)

	fmt.Println("Entry updated successfully")
}

// Utility functions
//==============================================================================

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
