BUILD-DIR=../build

NIX-DIR := $(CURDIR)/../../../../nix
NIX-EXPRESSIONS := $(NIX-DIR)
NIX-GC-ROOT-DIR := $(BUILD-DIR)/nix-gc-roots
NIX-DERIVATION := experiments-haskell

.PHONY: all

all: compile

compile: src/hello.hs
	ghc src/hello.hs
	mv src/hello ../build
	mv src/*.hi src/*.o $(BUILD-DIR)

nix-shell:
	nix-shell					\
	  --pure					\
	  --indirect --add-root $(NIX-GC-ROOT-DIR)	\
	  -A $(NIX-DERIVATION)				\
	  $(NIX-EXPRESSIONS)
