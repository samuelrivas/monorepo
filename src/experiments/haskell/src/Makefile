BUILD-DIR := ../build
LIB-DIR := $(BUILD-DIR)/lib
BIN-DIR := $(BUILD-DIR)/bin
GEN-DIR := $(BUILD-DIR)/generated

NIX-DIR := $(CURDIR)/../../../../nix
NIX-EXPRESSIONS := $(NIX-DIR)
NIX-GC-ROOT-DIR := $(BUILD-DIR)/nix-gc-roots/nix-gc-roots
NIX-GC-ROOTS := $(NIX-GC-ROOT-DIR)/gc-roots
NIX-DERIVATION := "with import $(NIX-DIR) {}; experiments-haskell.override {sandbox = true;}"

HS-FILES := $(wildcard src/*.hs)
OBJECT-FILES := $(addprefix $(LIB-DIR)/, $(notdir $(HS-FILES:.hs=.o)))
DEPENDENCIES-FILE := $(GEN-DIR)/dependencies.mk

GHC-FLAGS := -Wall -outputdir $(LIB-DIR)
GHC-LD-FLAGS := -Wall
GHC := ghc $(GHC-FLAGS)

GHC-PACKAGES := containers
GHC-LD := ghc $(GHC-LD-FLAGS) $(addprefix -package , $(GHC-PACKAGES))

PROGRAM := $(BIN-DIR)/main

.PHONY: all compile clean

all: $(PROGRAM)

$(LIB-DIR):
	mkdir -p $@

$(BIN-DIR):
	mkdir -p $@

$(NIX-GC-ROOT-DIR):
	mkdir -p $@

$(GEN-DIR):
	mkdir -p $@

$(LIB-DIR)/%.hi: $(LIB-DIR)/%.o
	@:

$(LIB-DIR)/%.o: src/%.hs
	$(GHC) -i$(LIB-DIR) -c $<

clean:
	rm -rf $(BUILD-DIR)

compile: $(OBJECT-FILES)

$(PROGRAM): $(BIN-DIR) $(OBJECT-FILES)
	$(GHC-LD) $(OBJECT-FILES) -o $@

nix-shell: $(NIX-GC-ROOT-DIR)
	nix-shell					\
	  --pure					\
	  --indirect --add-root $(NIX-GC-ROOTS)		\
	  -E $(NIX-DERIVATION)				\
	  $(NIX-EXPRESSIONS)

$(DEPENDENCIES-FILE): $(HS-FILES) $(GEN-DIR)
	$(GHC) -dep-suffix '' -dep-makefile $@ -M $(HS-FILES)

include $(DEPENDENCIES-FILE)
