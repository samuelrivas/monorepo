package main

import (
	"fmt"
	"io"
	"log/slog"
	"os"

	"filippo.io/age"
	"golang.org/x/term"
)

func askForSafeInput(fieldName string) string {
	fmt.Printf("Enter %s: ", fieldName)
	password, err := term.ReadPassword(int(os.Stdin.Fd()))
	if err != nil {
		errorMessageLn("Error reading '%s'", fieldName)
		panic(err)
	}
	fmt.Println()
	return string(password)
}

// Decrypt filePath and return a io.Reader on the clear text. It returns
// the identity as well so that we can re-encrpyt with it
func getCleartext(filename string) (string, string) {
	fd := openFileRead(filename)
	defer fd.Close()

	password := askForSafeInput("Password to unlock input file")

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
