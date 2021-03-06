# Interface
# =========

# PREFIX can be set to install in a different place. By default we install in
# $(BUILD-DIR)/install

PACKAGE-NAME := adventlib
PACKAGE-VERSION := 1.0

GHC-FLAGS := -O3 -Wall -j

# Internal variables
# ==================
# TODO: Many of these must be generalised when improving this Makefile
GHC-VERSION := 8.6.5
ARCH = x86_64-linux

BUILD-DIR := ../build
BUILD-OUTPUT-DIR := $(BUILD-DIR)/out
INSTALL-DIR = $(PREFIX)
GENERATED-DIR := $(BUILD-DIR)/generated

SRCS := $(shell find . -name "*.hs")
SRC-NAMES = $(SRCS:./src/%=./%)
OBJ-NAMES = $(SRC-NAMES:%.hs=%.o)

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

PACKAGE-DEPS := $(GENERATED-DIR)/package-deps
PACKAGE-CONF := $(PACKAGE-CONF-DIR)/$(PACKAGE-NAME).conf

# Targets
# =======
.PHONY: all
all: compile

$(PACKAGE-DEPS): templates/package-deps.template | $(GENERATED-DIR)
	cat $< | xargs -L1 -I{} ghc-pkg field "{}" id | \
	awk -e '{print "    "$$NF}' > $@

$(PACKAGE-CONF): templates/$(PACKAGE-NAME).conf.template $(PACKAGE-DEPS) | $(PACKAGE-CONF-DIR) $(INSTALLED-DYNAMIC-LIB-DIR) $(INSTALLED-STATIC-LIB-DIR)
	sed -e "s|@@STATIC-LIB-DIR@@|$(realpath $(INSTALLED-STATIC-LIB-DIR))|; \
	        s|@@IMPORT-DIR@@|$(realpath $(INSTALLED-STATIC-LIB-DIR))|; \
		s|@@DYNAMIC-LIB-DIR@@|$(realpath $(INSTALLED-DYNAMIC-LIB-DIR))|" \
	< $< > $@
	cat $(PACKAGE-DEPS) >> $@

.PHONY: compile
compile: | $(DYNAMIC-LIB-DIR) $(STATIC-LIB-DIR)
	cd src; ghc $(GHC-FLAGS) -outputdir $(realpath $(STATIC-LIB-DIR)) --make -dynamic -shared -fPIC -package-name $(PACKAGE-NAME) $(SRC-NAMES) -osuf dyn_o -hisuf dyn_hi -o libHS$(PACKAGE-NAME)-ghc$(GHC-VERSION).so
	cd src; ghc $(GHC-FLAGS) -c --make -outputdir $(realpath $(STATIC-LIB-DIR)) -package-name $(PACKAGE-NAME) $(SRC-NAMES)
	mv src/*.so $(DYNAMIC-LIB-DIR)
	ar cqs $(STATIC-LIB-DIR)/libHS$(PACKAGE-NAME).a $(addprefix $(STATIC-LIB-DIR)/,$(OBJ-NAMES))

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
