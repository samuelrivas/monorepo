# Interface
# =========

# PREFIX can be set to install in a different place. By default we install in
# $(BUILD-DIR)/install

PACKAGE-NAME := adventlib
PACKAGE-VERSION := 1.0

# Internal variables
# ==================
# TODO: Many of these must be generalised when improving this Makefile
GHC-VERSION := 8.6.5
ARCH = x86_64-linux

BUILD-DIR := ../build
BUILD-OUTPUT-DIR := $(BUILD-DIR)/out
INSTALL-DIR = $(PREFIX)

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

$(PACKAGE-CONF): templates/$(PACKAGE-NAME).conf.template | $(PACKAGE-CONF-DIR) $(INSTALLED-DYNAMIC-LIB-DIR) $(INSTALLED-STATIC-LIB-DIR)
	sed -e "s|@@STATIC-LIB-DIR@@|$(realpath $(INSTALLED-STATIC-LIB-DIR))|; \
	        s|@@IMPORT-DIR@@|$(realpath $(INSTALLED-STATIC-LIB-DIR))|; \
		s|@@DYNAMIC-LIB-DIR@@|$(realpath $(INSTALLED-DYNAMIC-LIB-DIR))|" \
	< $< > $@

.PHONY: compile
compile: | $(DYNAMIC-LIB-DIR) $(STATIC-LIB-DIR)
	cd src; ghc -outputdir $(realpath $(STATIC-LIB-DIR)) --make -dynamic -shared -fPIC -package-name adventlib System/IO/Advent.hs System/IO/Test.hs -osuf dyn_o -hisuf dyn_hi -o libHSadventlib-ghc8.6.5.so
	cd src; ghc -c --make -outputdir $(realpath $(STATIC-LIB-DIR)) -package-name adventlib System/IO/Advent.hs System/IO/Test.hs
	mv src/*.so $(DYNAMIC-LIB-DIR)
	ar cqs $(STATIC-LIB-DIR)/libHSadventlib.a $(STATIC-LIB-DIR)/System/IO/*.o

.PHONY: test
test:
	# ghc --make src/MainTest.hs
	# ghc --make -dynamic src/MainTest.hs -o src/MainTest_dyn
	# src/MainTest
	# src/MainTest_dyn

.PHONY: clean
clean:
	rm -rf $(BUILD-DIR)

.PHONY: install
install: compile $(PACKAGE-CONF)
	cd $(BUILD-DIR)/out; tar c --exclude "*.o" --exclude "*.dyn_o" * | tar x -C $(INSTALL-DIR)

$(PACKAGE-DIR) \
$(PACKAGE-CONF-DIR) \
$(INSTALL-DIR) \
$(DYNAMIC-LIB-DIR) \
$(STATIC-LIB-DIR) \
$(INSTALLED-DYNAMIC-LIB-DIR) \
$(INSTALLED-STATIC-LIB-DIR) :
	mkdir -p $@
