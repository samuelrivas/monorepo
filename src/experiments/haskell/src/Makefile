BUILD-DIR=../build

NIX-DIR := $(CURDIR)/../../../../nix
NIX-EXPRESSIONS := $(NIX-DIR)
NIX-GC-ROOT-DIR := $(BUILD-DIR)/nix-gc-roots
NIX-DERIVATION := "with import $(NIX-DIR) {}; experiments-haskell.override {sandbox = true;}"

.PHONY: all compile

all: compile

$(BUILD-DIR):
	mkdir -p $(BUILD-DIR)/bin
	mkdir -p $(BUILD-DIR)/lib

compile: $(BUILD-DIR) src/hello.hs
	ghc src/hello.hs
	mv src/hello $(BUILD-DIR)/bin
	mv src/*.hi src/*.o $(BUILD-DIR)/lib

nix-shell:
	nix-shell					\
	  --pure					\
	  --indirect --add-root $(NIX-GC-ROOT-DIR)	\
	  -E $(NIX-DERIVATION)				\
	  $(NIX-EXPRESSIONS)
