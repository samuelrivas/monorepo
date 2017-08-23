BUILD-DIR := ../build
LIB-DIR := $(BUILD-DIR)/lib
BIN-DIR := $(BUILD-DIR)/bin
GEN-DIR := $(BUILD-DIR)/generated

HS-FILES := $(wildcard src/*.hs)
OBJECT-FILES := $(addprefix $(LIB-DIR)/, $(notdir $(HS-FILES:.hs=.o)))
DEPENDENCIES-FILE := $(GEN-DIR)/dependencies.mk

GHC-FLAGS := -Wall -outputdir $(LIB-DIR)
GHC-LD-FLAGS := -Wall
GHC := ghc $(GHC-FLAGS)

GHC-PACKAGES := text mtl
GHC-LD := ghc $(GHC-LD-FLAGS) $(addprefix -package , $(GHC-PACKAGES))

PROGRAM := $(BIN-DIR)/text-to-char-context

.PHONY: all compile clean

all: $(PROGRAM)

$(LIB-DIR):
	mkdir -p $@

$(BIN-DIR):
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

$(DEPENDENCIES-FILE): $(HS-FILES) $(GEN-DIR)
	$(GHC) -dep-suffix '' -dep-makefile $@ -M $(HS-FILES)

include $(DEPENDENCIES-FILE)
