BUILD-DIR := ../build
LIB-DIR := $(BUILD-DIR)/lib
BIN-DIR := $(BUILD-DIR)/bin

NIX-DIR := $(CURDIR)/../../../../nix
NIX-EXPRESSIONS := $(NIX-DIR)
NIX-GC-ROOT-DIR := $(BUILD-DIR)/nix-gc-roots/nix-gc-roots
NIX-GC-ROOTS := $(NIX-GC-ROOT-DIR)/gc-roots
NIX-DERIVATION := "with import $(NIX-DIR) {}; experiments-haskell.override {sandbox = true;}"

HS-FILES := $(wildcard src/*.hs)
OBJECT-FILES := $(addprefix $(LIB-DIR)/, $(notdir $(HS-FILES:.hs=.o)))

GHC-FLAGS := -Wall
GHC := ghc $(GHC-FLAGS)

.PHONY: all compile clean

all: compile

$(LIB-DIR):
	mkdir -p $@

$(BIN-DIR):
	mkdir -p $@

$(NIX-GC-ROOT-DIR):
	mkdir -p $@

$(LIB-DIR)/%.hi: $(LIB-DIR)/%.o
	@:

$(LIB-DIR)/%.o: src/%.hs
	$(GHC) -i$(LIB-DIR) -c $< -odir $(LIB-DIR) -hidir $(LIB-DIR)

clean:
	rm -rf $(BUILD-DIR)

# compile: $(BUILD-DIR) src/main.hs
# 	ghc src/main.hs
# 	mv src/main $(BUILD-DIR)/bin
# 	mv src/*.hi src/*.o $(BUILD-DIR)/lib

compile: $(OBJECT-FILES)

nix-shell: $(NIX-GC-ROOT-DIR)
	nix-shell					\
	  --pure					\
	  --indirect --add-root $(NIX-GC-ROOTS)		\
	  -E $(NIX-DERIVATION)				\
	  $(NIX-EXPRESSIONS)
