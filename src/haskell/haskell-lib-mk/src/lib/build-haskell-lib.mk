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

GHC-FLAGS += -O3 -Wall -j
GHC-PROF-FLAGS += -O3 -Wall -j -prof

# Internal variables
# ==================
GHC-VERSION := $(shell ghc --numeric-version)
ARCH = x86_64-linux

BUILD-DIR := ../build
BUILD-OUTPUT-DIR := $(BUILD-DIR)/out
PREFIX ?= $(BUILD-DIR)/install
PREFIX-DOCS ?= $(BUILD-DIR)/install
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

DOC-OUTPUT-DIR := $(BUILD-DIR)/out-doc
DOC-RELATIVE-DIR := $(PACKAGE-NAME)/html
DOC-INSTALL-DIR ?= $(PREFIX-DOCS)/share/doc
BUILD-DOC-DIR := $(DOC-OUTPUT-DIR)/$(DOC-RELATIVE-DIR)
INSTALLED-DOC-HTML := $(DOC-INSTALL-DIR)/$(DOC-RELATIVE-DIR)

# Some packages export the `haddock-html` field even when the docs are not built, haddock
# fails to build if we pass a non-existent directory to the `-i` flag, so we need to filter
# out with `realpath`
HADDOCK-HTML-DIRS-RAW := $(shell ghc-pkg field '*' haddock-html --simple-output)
HADDOCK-HTML-DIRS := $(foreach dir,$(HADDOCK-HTML-DIRS-RAW),$(realpath $(dir)))

# Finding the haddock file with a star is not correct, though it works in
# practice, what we want is the haddock-interfaces field. An option would be to
# read dep by dep and use the correct fields, but that is slow, and makes it
# harder to filter out documentation directories that don't exist. A better
# option is wo write a tool to generate these flags. As this is fast adn works
# in practice, we will leave it like this for now.
HADDOCK-INTERFACE-FLAGS := $(foreach dir,$(HADDOCK-HTML-DIRS),-i file://$(dir),file://$(dir)/src,$(wildcard $(dir)/*.haddock))

# Targets
# =======
.SUFFIXES:

.PHONY: all
all: compile

# We create the installed doc directory conditionally, as destination may not
# exist if we are not installing the docs
$(PACKAGE-CONF): | $(PACKAGE-CONF-DIR) $(INSTALLED-DYNAMIC-LIB-DIR) $(INSTALLED-STATIC-LIB-DIR) $(if $(PREFIX-DOCS),$(INSTALLED-DOC-HTML),)
	mk-conf-file $(PACKAGE-NAME) \
		--version "$(PACKAGE-VERSION)" \
		--exposed "$(EXPOSED-MODULES)" \
		--import-path "$(realpath $(INSTALLED-STATIC-LIB-DIR))" \
		--static-lib-path "$(realpath $(INSTALLED-STATIC-LIB-DIR))" \
		--dynamic-lib-path "$(realpath $(INSTALLED-DYNAMIC-LIB-DIR))" \
		--dependencies "$(PACKAGE-DEPS)" \
		$(if $(PREFIX-DOCS),--haddock-interfaces "$(realpath $(INSTALLED-DOC-HTML))/$(PACKAGE-NAME).haddock",) \
		$(if $(PREFIX-DOCS),--haddock-html "$(realpath $(INSTALLED-DOC-HTML))") \
		> $@

.PHONY: compile
compile: | $(DYNAMIC-LIB-DIR) $(STATIC-LIB-DIR)
	cd src; ghc $(GHC-FLAGS) -outputdir $(realpath $(STATIC-LIB-DIR)) --make -dynamic -shared -fPIC -package-name $(PACKAGE-NAME) $(SRC-NAMES) -osuf dyn_o -hisuf dyn_hi -o libHS$(PACKAGE-NAME)-ghc$(GHC-VERSION).so
	cd src; ghc $(GHC-FLAGS) -c --make -outputdir $(realpath $(STATIC-LIB-DIR)) -package-name $(PACKAGE-NAME) $(SRC-NAMES)
	cd src; ghc $(GHC-PROF-FLAGS) -c --make -outputdir $(realpath $(STATIC-LIB-DIR)) -package-name $(PACKAGE-NAME) $(SRC-NAMES) -osuf p_o -hisuf p_hi
	mv src/*.so $(DYNAMIC-LIB-DIR)
	ar cqs $(STATIC-LIB-DIR)/libHS$(PACKAGE-NAME).a $(addprefix $(STATIC-LIB-DIR)/,$(OBJ-NAMES))
	ar cqs $(STATIC-LIB-DIR)/libHS$(PACKAGE-NAME)_p.a $(addprefix $(STATIC-LIB-DIR)/,$(PROF-OBJ-NAMES))

.PHONY: doc
doc: | $(BUILD-DOC-DIR)
	haddock \
	--html \
	--hoogle \
	--hyperlinked-source \
	--odir $(BUILD-DOC-DIR) \
	--package-name $(PACKAGE-NAME) \
	--quickjump \
	--dump-interface=$(BUILD-DOC-DIR)/$(PACKAGE-NAME).haddock \
	$(HADDOCK-INTERFACE-FLAGS) \
	$(SRCS)

.PHONY: clean
clean:
	rm -rf $(BUILD-DIR)

.PHONY: install
install: compile $(PACKAGE-CONF) | $(INSTALL-DIR)
	cd $(BUILD-OUTPUT-DIR); tar c --exclude "*.o" --exclude "*.dyn_o" * \
		| tar x -C $(realpath $(INSTALL-DIR))

.PHONY: install-doc
install-doc: doc $(PACKAGE-CONF) | $(DOC-INSTALL-DIR)
	cp -r $(BUILD-DOC-DIR)/* $(INSTALLED-DOC-HTML)

$(PACKAGE-DIR) \
$(PACKAGE-CONF-DIR) \
$(INSTALL-DIR) \
$(GENERATED-DIR) \
$(DYNAMIC-LIB-DIR) \
$(STATIC-LIB-DIR) \
$(INSTALLED-DYNAMIC-LIB-DIR) \
$(INSTALLED-STATIC-LIB-DIR) \
$(DOC-INSTALL-DIR) \
$(BUILD-DOC-DIR) \
$(INSTALLED-DOC-HTML):
	mkdir -p $@
