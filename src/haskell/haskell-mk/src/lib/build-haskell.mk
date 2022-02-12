# Common makefile for all projects living in this monorepo
#
# Heavily based on our own conventions. Namely
#   - All files are generated in a separate directory (BUILD-DIR)
#   - All source files are in the src dir
#   - One executable per project
#   - No cabal, we compile and link with ghc, like the good old times :P
#   - Control from the outside via PROGRAM-NAME, GHC-FLAGS, GHC-LD-FLAGS and
#     GHC-PACKAGES

# Interface
MAIN-MODULE ?= src/Main.hs
PROGRAM-NAME ?= main
GHC-FLAGS ?=

# Internal variables
BUILD-DIR := ../build
BIN-DIR := $(BUILD-DIR)/bin
GEN-DIR := $(BUILD-DIR)/generated

GHC-FLAGS += -isrc -j -outputdir $(GEN-DIR)
GHC := ghc $(GHC-FLAGS)

PROGRAM := $(BIN-DIR)/$(PROGRAM-NAME)

.PHONY: all
all: $(PROGRAM) $(INPUT-FILES)

$(BIN-DIR):
	mkdir -p $@

$(GEN-DIR):
	mkdir -p $@

$(LIB-DIR):
	mkdir -p $@

.PHONY: clean
clean:
	rm -rf $(BUILD-DIR)

.PHONY: $(PROGRAM)
$(PROGRAM): | $(BIN-DIR)
	$(GHC) $(MAIN-MODULE) -o $@
