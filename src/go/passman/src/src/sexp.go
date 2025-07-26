package main

import (
	"fmt"
	"log/slog"
	"strings"

	"github.com/samuelrivas/monorepo/passman/internal/sets"
)

func runSexp(
	command, query, document string,
	sensitive bool) (string, string, int) {
	stdout, stderr, status := runCommand(
		"sexp", []string{command, query}, document, sensitive)

	return stdout.String(), stderr.String(), status
}

func runSexpChange(query, document string, sensitive bool) string {
	output, _, status := runSexp("change", query, document, sensitive)
	validateExitStatus("sexp change", status)
	return output
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

// Query that given an object as input returns a list with the values of the
// specified fields. If only one field is specified, it returns the value as
// atom instead
func presentFieldsQuery(fields []string) string {
	fieldSexps := make([]string, 0, len(fields))
	for _, arg := range fields {
		fieldSexps = append(fieldSexps, fmt.Sprintf("(field %s)", arg))
	}

	allFields := strings.Join(fieldSexps, " ")
	if len(fields) > 1 {
		return fmt.Sprintf("(wrap (cat %s))", allFields)
	} else {
		return allFields
	}
}

// Update fields of a site, existingFields must be fields that are already
// present in the site record, and newFiles must be fields that aren't present
// in the site record.
func updateFields(
	cleartext,
	site string,
	existingFields map[string]string,
	newFields map[string]string,
	sensitive bool) string {

	fieldLhs := make(map[string]string)
	for k := range existingFields {
		fieldLhs[k] = "$" + k
	}

	changeMatch := toSplice(fieldLhs)
	changeValue := toSplice(existingFields) + toSplice(newFields)
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
func getSiteFieldNames(cleartext, site string) sets.Set {
	query := fmt.Sprintf(
		"each (test (field site) (equals \"%s\")) each (index 0)",
		site)
	output := runSexpQuery(query, cleartext, false)

	fields := sets.Make()
	for v := range strings.SplitSeq(output, "\n") {
		sets.Add(v, fields)
	}
	return fields
}

// Convert a map into a string with a sepx list
func toSexp(x map[string]string) string {
	return fmt.Sprintf("(%s)", toSplice(x))
}

// Convert a map into a string with a sexp splice
func toSplice(x map[string]string) string {
	out := ""

	for k, v := range x {
		out += fmt.Sprintf("(%s \"%s\")", k, v)
	}
	return out
}
