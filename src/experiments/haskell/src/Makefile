.PHONY: all

all: compile

compile: src/hello.hs
	ghc src/hello.hs
	mv src/hello ../build
	mv src/*.hi src/*.o ../build
