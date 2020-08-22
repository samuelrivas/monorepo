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
INSTALL-DIR = $(PREFIX)
PACKAGE-DIR := $(BUILD-DIR)/out/lib/ghc-$(GHC-VERSION)
INSTALL-PACKAGE-DIR := $(INSTALL-DIR)/lib/ghc-$(GHC-VERSION)

PACKAGE-CONF-DIR := $(PACKAGE-DIR)/package.conf.d
INSTALL-PACKAGE-CONF-DIR := $(INSTALL-PACKAGE-DIR)/package.conf.d
DYNAMIC-LIB-DIR := $(PACKAGE-DIR)/$(ARCH)-ghc-$(GHC-VERSION)
STATIC-LIB-DIR := $(PACKAGE-DIR)/$(ARCH)-ghc-$(GHC-VERSION)/$(PACKAGE-NAME)-$(PACKAGE-VERSION)
INSTALLED-DYNAMIC-LIB-DIR := $(INSTALL-DIR)/$(ARCH)-ghc-$(GHC-VERSION)
INSTALLED-STATIC-LIB-DIR := $(INSTALL-DIR)/$(ARCH)-ghc-$(GHC-VERSION)/$(PACKAGE-NAME)-$(PACKAGE-VERSION)

PACKAGE-CONF := $(PACKAGE-CONF-DIR)/$(PACKAGE-NAME).conf
INSTALLED-PACKAGE-CONF := $(INSTALL-PACKAGE-CONF-DIR)/$(PACKAGE-NAME).conf

# Targets
# =======
$(PACKAGE-DIR) \
$(INSTALL-DIR) \
$(PACKAGE-CONF-DIR) \
$(INSTALL-PACKAGE-CONF-DIR) \
$(DYNAMIC-LIB-DIR) \
$(STATIC-LIB-DIR):
	mkdir -p $@

.PHONY: all
all: lib $(PACKAGE-CONF)

$(PACKAGE-CONF): templates/$(PACKAGE-NAME).conf.template | $(PACKAGE-CONF-DIR) $(DYNAMIC-LIB-DIR) $(STATIC-LIB-DIR)
	sed -e "s|@@INSTALL-DIR-INTERFACE@@|$(realpath $(STATIC-LIB-DIR))|; \
		s|@@INSTALL-DIR-BINARIES@@|$(realpath $(DYNAMIC-LIB-DIR))|" \
	< $< > $@

$(INSTALLED-PACKAGE-CONF): templates/$(PACKAGE-NAME).conf.template | $(PACKAGE-CONF-DIR) $(DYNAMIC-LIB-DIR) $(STATIC-LIB-DIR)
	sed -e "s|@@INSTALL-DIR-INTERFACE@@|$(realpath $(INSTALLED-STATIC-LIB-DIR))|; \
		s|@@INSTALL-DIR-BINARIES@@|$(realpath $(INSTALLED-DYNAMIC-LIB-DIR))|" \
	< $< > $@

.PHONY: lib
lib: | $(DYNAMIC-LIB-DIR) $(STATIC-LIB-DIR)
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
install: lib
	cd $(BUILD-DIR)/out; tar c --exclude "*.o" --exclude "*.dyn_o" * | tar x -C $(INSTALL-DIR)
install-clean:
	rm -r $(INSTALL-PATH)/*
