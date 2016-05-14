BUILD-DIR=../build

NIX-DIR := $(CURDIR)/../../../../nix
NIX-EXPRESSIONS := $(NIX-DIR)
NIX-GC-ROOT-DIR := $(BUILD-DIR)/nix-gc-roots
NIX-DERIVATION := "with import $(NIX-DIR) {}; experiments-haskell.override {sandbox = true;}"

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
	  -E $(NIX-DERIVATION)				\
	  $(NIX-EXPRESSIONS)
