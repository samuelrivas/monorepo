# Common makefile for all library projects living in this monorepo
#
# Heavily based on our own conventions. Namely
#   - All files are generated in a separate directory (BUILD-DIR)
#   - All source files are in the src dir
#   - One package per project
#   - No cabal, we compile and link with ghc, like the good old times :P
#   - Packages are listed explicitly (this is not necessarily the best option,
#     since it duplicates information that is already in the nix derivation, but
#     such is life for now
#   - Control from the outside via PROGRAM-NAME, GHC-FLAGS, GHC-LD-FLAGS and
#     GHC-PACKAGES
#   - The main test file is test.sh and any other test module is in the Test
#     namespace

# Interface
# =========

# PREFIX can be set to install in a different place. By default we install in
# $(BUILD-DIR)/install

PACKAGE-NAME ?= $(error "you must define PACKAGE-NAME")
PACKAGE-VERSION ?= 1.0
PACKAGE-DEPS ?= $(error "you must define PACKAGE-DEPS")
EXPOSED-MODULES ?= $(error "you must define PACKAGE-MODULES")

GHC-FLAGS ?= -O3 -Wall -j
GHC-PROF-FLAGS ?= -O3 -Wall -j -prof

# Internal variables
# ==================
GHC-VERSION := $(shell ghc --numeric-version)
ARCH = x86_64-linux

BUILD-DIR := ../build
BUILD-OUTPUT-DIR := $(BUILD-DIR)/out
INSTALL-DIR = $(PREFIX)
GENERATED-DIR := $(BUILD-DIR)/generated

SRCS := $(shell find ./src -name "*.hs")
SRC-NAMES = $(filter-out ./test.hs ./Test/%,$(SRCS:./src/%=./%))
OBJ-NAMES = $(SRC-NAMES:%.hs=%.o)
PROF-OBJ-NAMES = $(SRC-NAMES:%.hs=%.p_o)

PACKAGE-RELATIVE-DIR := lib/ghc-$(GHC-VERSION)
PACKAGE-CONF-RELATIVE-DIR := lib/ghc-$(GHC-VERSION)/package-conf.d
DYNAMIC-LIBRARY-RELATIVE-DIR := $(PACKAGE-RELATIVE-DIR)/$(ARCH)-ghc-$(GHC-VERSION)
STATIC-LIBRARY-RELATIVE-DIR := $(PACKAGE-RELATIVE-DIR)/$(ARCH)-ghc-$(GHC-VERSION)/$(PACKAGE-NAME)-$(PACKAGE-VERSION)

BUILD-PACKAGE-DIR := $(BUILD-OUTPUT-DIR)/$(PACKAGE-RELATIVE-DIR)
INSTALL-PACKAGE-DIR := $(INSTALL-DIR)/$(PACKAGE-RELATIVE-DIR)

PACKAGE-CONF-DIR := $(INSTALL-PACKAGE-DIR)/package.conf.d
DYNAMIC-LIB-DIR := $(BUILD-PACKAGE-DIR)/$(ARCH)-ghc-$(GHC-VERSION)
STATIC-LIB-DIR := $(BUILD-PACKAGE-DIR)/$(ARCH)-ghc-$(GHC-VERSION)/$(PACKAGE-NAME)-$(PACKAGE-VERSION)
INSTALLED-DYNAMIC-LIB-DIR := $(INSTALL-PACKAGE-DIR)/$(ARCH)-ghc-$(GHC-VERSION)
INSTALLED-STATIC-LIB-DIR := $(INSTALL-PACKAGE-DIR)/$(ARCH)-ghc-$(GHC-VERSION)/$(PACKAGE-NAME)-$(PACKAGE-VERSION)

PACKAGE-CONF := $(PACKAGE-CONF-DIR)/$(PACKAGE-NAME).conf

# Targets
# =======
.PHONY: all
all: compile

$(PACKAGE-CONF): | $(PACKAGE-CONF-DIR) $(INSTALLED-DYNAMIC-LIB-DIR) $(INSTALLED-STATIC-LIB-DIR)
	mk-conf-file $(PACKAGE-NAME) \
		--version "$(PACKAGE-VERSION)" \
		--exposed "$(EXPOSED-MODULES)" \
		--import-path "$(realpath $(INSTALLED-STATIC-LIB-DIR))" \
		--static-lib-path "$(realpath $(INSTALLED-STATIC-LIB-DIR))" \
		--dynamic-lib-path "$(realpath $(INSTALLED-DYNAMIC-LIB-DIR))" \
		--dependencies "$(PACKAGE-DEPS)" \
		> $@

.PHONY: compile
compile: | $(DYNAMIC-LIB-DIR) $(STATIC-LIB-DIR)
	cd src; ghc $(GHC-FLAGS) -outputdir $(realpath $(STATIC-LIB-DIR)) --make -dynamic -shared -fPIC -package-name $(PACKAGE-NAME) $(SRC-NAMES) -osuf dyn_o -hisuf dyn_hi -o libHS$(PACKAGE-NAME)-ghc$(GHC-VERSION).so
	cd src; ghc $(GHC-FLAGS) -c --make -outputdir $(realpath $(STATIC-LIB-DIR)) -package-name $(PACKAGE-NAME) $(SRC-NAMES)
	cd src; ghc $(GHC-PROF-FLAGS) -c --make -outputdir $(realpath $(STATIC-LIB-DIR)) -package-name $(PACKAGE-NAME) $(SRC-NAMES) -osuf p_o -hisuf p_hi
	mv src/*.so $(DYNAMIC-LIB-DIR)
	ar cqs $(STATIC-LIB-DIR)/libHS$(PACKAGE-NAME).a $(addprefix $(STATIC-LIB-DIR)/,$(OBJ-NAMES))
	ar cqs $(STATIC-LIB-DIR)/libHS$(PACKAGE-NAME)_p.a $(addprefix $(STATIC-LIB-DIR)/,$(PROF-OBJ-NAMES))

.PHONY: clean
clean:
	rm -rf $(BUILD-DIR)

.PHONY: install
install: compile $(PACKAGE-CONF)
	cd $(BUILD-DIR)/out; tar c --exclude "*.o" --exclude "*.dyn_o" * | tar x -C $(INSTALL-DIR)

$(PACKAGE-DIR) \
$(PACKAGE-CONF-DIR) \
$(INSTALL-DIR) \
$(GENERATED-DIR) \
$(DYNAMIC-LIB-DIR) \
$(STATIC-LIB-DIR) \
$(INSTALLED-DYNAMIC-LIB-DIR) \
$(INSTALLED-STATIC-LIB-DIR) :
	mkdir -p $@
