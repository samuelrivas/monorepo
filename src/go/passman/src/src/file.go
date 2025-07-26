package main

import (
	"io"
	"log/slog"
	"os"
)

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

func writeToFile(filename string, fd *os.File) {
	destFd := openFileWrite(filename)
	defer destFd.Close()

	_, err := io.Copy(destFd, fd)
	if err != nil {
		errorMessageLn("Error copying %s to %s", fd.Name(), filename)
		panic(err)
	}
}

