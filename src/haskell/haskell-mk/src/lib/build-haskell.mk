# Common makefile for all projects living in this monorepo
#
# Heavily based on our own conventions. Namely
#   - All files are generated in a separate directory (BUILD-DIR)
#   - One executable per project
#   - No cabal, we compile and link with ghc, like the good old times :P
#   - Packages are listed explicitly (this is not necessarily the best option,
#     since it duplicates information that is already in the nix derivation, but
#     such is life for now
#   - Control from the outside via PROGRAM-NAME, GHC-FLAGS, GHC-LD-FLAGS and
#     GHC-PACKAGES

# Interface
GHC-PACKAGES ?=
PROGRAM-NAME ?= main
GHC-FLAGS ?= -Wall
GHC-LD-FLAGS ?= -Wall

# Internal variables
BUILD-DIR := ../build
LIB-DIR := $(BUILD-DIR)/lib
BIN-DIR := $(BUILD-DIR)/bin
GEN-DIR := $(BUILD-DIR)/generated

HS-FILES := $(wildcard src/*.hs)
OBJECT-FILES := $(addprefix $(LIB-DIR)/, $(notdir $(HS-FILES:.hs=.o)))
DEPENDENCIES-FILE := $(GEN-DIR)/dependencies.mk

# Profiler
GHC-FLAGS += -outputdir $(LIB-DIR)
GHC := ghc $(GHC-FLAGS)

GHC-LD := ghc $(GHC-LD-FLAGS) $(addprefix -package , $(GHC-PACKAGES))

PROGRAM := $(BIN-DIR)/$(PROGRAM-NAME)

.PHONY: all
all: $(PROGRAM)

$(LIB-DIR):
	mkdir -p $@

$(BIN-DIR):
	mkdir -p $@

$(GEN-DIR):
	mkdir -p $@

$(LIB-DIR)/%.hi: $(LIB-DIR)/%.o
	@:

$(LIB-DIR)/%.o: src/%.hs | $(LIB-DIR)
	$(GHC) -i$(LIB-DIR) -c $<

.PHONY: clean
clean:
	rm -rf $(BUILD-DIR)

.PHONY: compile
compile: $(OBJECT-FILES)

$(PROGRAM): $(OBJECT-FILES) | $(BIN-DIR)
	$(GHC-LD) $(OBJECT-FILES) -o $@

$(DEPENDENCIES-FILE): $(HS-FILES) | $(GEN-DIR)
	$(GHC) -dep-suffix '' -dep-makefile $@ -M $(HS-FILES)

include $(DEPENDENCIES-FILE)
