BUILD-DIR := ../build
LIB-DIR := $(BUILD-DIR)/lib
BIN-DIR := $(BUILD-DIR)/bin
GEN-DIR := $(BUILD-DIR)/generated

MAIN-HS-FILES := $(wildcard src/*.hs)

## outputdir doesn't fully work with --make, fix this
GHC-FLAGS := -j -Wall -outputdir $(GEN-DIR)
GHC := ghc $(GHC-FLAGS)

GHC-PACKAGES := 

PROGRAMS := $(MAIN-HS-FILES:src%.hs=$(GEN-DIR)%)

.PHONY: all

all: $(PROGRAMS)

$(LIB-DIR):
	mkdir -p $@

$(BIN-DIR):
	mkdir -p $@

$(GEN-DIR):
	mkdir -p $@


.PHONY: clean
clean:
	rm -rf $(BUILD-DIR)

$(PROGRAMS): $(MAIN-HS-FILES) | $(GEN-DIR)
	$(GHC) --make $^

include $(DEPENDENCIES-FILE)
