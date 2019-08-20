BUILD-DIR := ../build
LIB-DIR := $(BUILD-DIR)/lib
BIN-DIR := $(BUILD-DIR)/bin
GEN-DIR := $(BUILD-DIR)/generated

HS-FILES := $(wildcard src/*.hs)
OBJECT-FILES := $(addprefix $(LIB-DIR)/, $(notdir $(HS-FILES:.hs=.o)))
DEPENDENCIES-FILE := $(GEN-DIR)/dependencies.mk

# Profiler
#GHC-FLAGS := -Wall -prof -fprof-auto -rtsopts -fexternal-interpreter -outputdir $(LIB-DIR)
GHC-FLAGS := -Wall -outputdir $(LIB-DIR)
GHC-LD-FLAGS := -Wall
GHC := ghc $(GHC-FLAGS)

GHC-PACKAGES := containers mtl random-fu multiset
GHC-LD := ghc $(GHC-LD-FLAGS) $(addprefix -package , $(GHC-PACKAGES))

PROGRAM := $(BIN-DIR)/main

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
