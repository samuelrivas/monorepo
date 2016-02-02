# Global configuraiton
BUILD-DIR := ../build

# Application stuff
SOURCES := $(shell find -name *.scala)
BUILD-JAR := $(BUILD-DIR)/samtime.jar

# Nix configuration
NIX-DIR := $(CURDIR)/../../../../nix
NIX-EXPRESSIONS := $(NIX-DIR)
NIX-GC-ROOT-DIR := $(BUILD-DIR)/nix-gc-roots
NIX-DERIVATION := samtime

# Configuration
VERBOSE_TYPES ?= 0
VERBOSE_IMPLICITS ?= 0

SCALAC_FLAGS :=

ifneq ($(VERBOSE_TYPES), 0)
SCALAC_FLAGS += -Xprint:typer
endif

ifneq ($(VERBOSE_IMPLICITS), 0)
SCALAC_FLAGS += -Xlog-implicits
endif

# Rules
.PHONY: all clean shell

all: $(BUILD-JAR)

$(BUILD-JAR): $(SOURCES)
	mkdir -p $(BUILD-DIR)
	scalac $(SCALAC_FLAGS) -d $@ $<

clean:

nix-shell:
	nix-shell					\
	  --pure					\
	  --indirect --add-root $(NIX-GC-ROOT-DIR)	\
	  -A $(NIX-DERIVATION)				\
	  $(NIX-EXPRESSIONS)
