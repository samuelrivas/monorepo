# Interface
MAIN-MODULE ?= Main.hs
PROGRAM-NAME ?= advent
GHC-FLAGS ?= -Wall -O3 -j -fno-warn-missing-signatures

# Internal variables
BUILD-DIR := ../build
BIN-DIR := $(BUILD-DIR)/bin
GEN-DIR := $(BUILD-DIR)/generated

GHC-FLAGS += -outputdir $(GEN-DIR)
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
