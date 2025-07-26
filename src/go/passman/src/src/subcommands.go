package main

import (
	"fmt"
	"log/slog"
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

	fields := toMap(parsedArgs.tailArgs)
	validateAddFields(fields)

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
