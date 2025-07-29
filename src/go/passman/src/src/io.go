package main

import (
	"bytes"
	"fmt"
	"log/slog"
	"os"
	"os/exec"

	// This is archived in favour of go's inferior "errors" package. If we
	// plan to implement more things here, consider implementing your own
	// errors with stack traces instead of depending on frozen external
	// package
	"github.com/pkg/errors"
)

func errorMessage(message string, args ...any) {
	if len(args) > 0 {
		message = fmt.Sprintf(message, args...)
	}
	fmt.Fprintf(os.Stderr, "%s", message)
}

func errorMessageLn(message string, args ...any) {
	errorMessage(message+"\n", args...)
}

func errorAndExit(message string, args ...any) {
	errorMessageLn(message, args...)
	os.Exit(1)
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
