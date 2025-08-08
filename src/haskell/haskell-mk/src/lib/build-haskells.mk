# Common makefile to build multiple binary haskell projects.
#
# Heavily based on our own conventions. Namely
#   - The binary Foo is built from a Foo.hs file exporting the module Foo
#   - All files are generated in a separate directory (BUILD-DIR)
#   - All source files are in the src dir
#   - No cabal, we compile and link with ghc, like the good old times :P
#   - Control from the outside via PROGRAM-NAME, GHC-FLAGS, GHC-LD-FLAGS and
#     GHC-PACKAGES

# Interface
MAIN-MODULES ?=$ (error "you must define MAIN-MODULES")
GHC-FLAGS ?=

# Internal variables
BUILD-DIR := ../build
BIN-DIR := $(BUILD-DIR)/bin
GEN-DIR := $(BUILD-DIR)/generated

GHC-FLAGS += -isrc -j -outputdir $(GEN-DIR)
GHC := ghc $(GHC-FLAGS)

PROGRAMS := $(basename $(notdir $(MAIN-MODULES)))
COMPILE-TARGETS := $(addprefix compile-,$(MAIN-MODULES))

$(warning $(MAIN-MODULES))
$(warning $(COMPILE-TARGETS))
.PHONY: all
all: $(COMPILE-TARGETS)

$(BIN-DIR):
	mkdir -p $@

$(GEN-DIR):
	mkdir -p $@

$(LIB-DIR):
	mkdir -p $@

.PHONY: clean
clean:
	rm -rf $(BUILD-DIR)

# These can, and need, to be phony since we want ghc to manage dependencies
#
# ghc compiles unnamed modules as Main by default, and also runs Main.main as
# main function. To compile multiple files, if all of them create the main
# module, Main.o file will get overwritten for every target but the first, so we
# will be recompiling all programs.
#
# We solve this by using -main-is, but this causes some trouble with standard
# tooling that expects Main to exist. We chose to use non-standard Main files
# because we recompile a lot, compared to the times when we would be annoyed by
# standard tooling
.PHONY: $(COMPILE-TARGETS)
$(COMPILE-TARGETS): | $(BIN-DIR)
	$(GHC) -main-is $(basename $(notdir $(subst compile-,,$@))) $(subst compile-,,$@) -o $(BIN-DIR)/$(basename $(notdir $(subst compile-,,$@)))
