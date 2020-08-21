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
PREFIX := $(BUILD-DIR)
BIN-DIR := $(BUILD-DIR)/bin
GEN-DIR := $(BUILD-DIR)/generated
INSTALL-DIR := $(PREFIX)/install/lib/ghc-$(GHC-VERSION)

INSTALL-DIR-PACKAGE-CONF := $(INSTALL-DIR)/package.conf.d
INSTALL-DIR-SO := $(INSTALL-DIR)/$(ARCH)-ghc-$(GHC-VERSION)
INSTALL-DIR-STATIC := $(INSTALL-DIR-SO)/$(PACKAGE-NAME)-$(PACKAGE-VERSION)

PACKAGE-CONF := $(INSTALL-DIR-PACKAGE-CONF)/$(PACKAGE-NAME).conf

# Targets
# =======
$(BIN-DIR) \
$(GEN-DIR) \
$(INSTALL-DIR) \
$(INSTALL-DIR-PACKAGE-CONF) \
$(INSTALL-DIR-SO) \
$(INSTALL-DIR-STATIC):
	mkdir -p $@

.PHONY: all
all: lib $(PACKAGE-CONF)

$(PACKAGE-CONF): templates/$(PACKAGE-NAME).conf.template | $(INSTALL-DIR-PACKAGE-CONF) $(INSTALL-DIR-STATIC) $(INSTALL-DIR-SO)
	sed -e "s|@@INSTALL-DIR-INTERFACE@@|$(realpath $(INSTALL-DIR-STATIC))|; \
		s|@@INSTALL-DIR-BINARIES@@|$(realpath $(INSTALL-DIR-SO))|" \
	< $< > $@

.PHONY: lib
lib: | $(INSTALL-DIR-STATIC)
	cd src; ghc -outputdir $(realpath $(INSTALL-DIR-STATIC)) --make -dynamic -shared -fPIC -package-name adventlib System/IO/Advent.hs System/IO/Test.hs -osuf dyn_o -hisuf dyn_hi -o libHSadventlib-ghc8.6.5.so
	cd src; ghc -c --make -outputdir $(realpath $(INSTALL-DIR-STATIC)) -package-name adventlib System/IO/Advent.hs System/IO/Test.hs
	mv src/*.so $(INSTALL-DIR-SO)
	ar cqs $(INSTALL-DIR-STATIC)/libHSadventlib.a $(INSTALL-DIR-STATIC)/System/IO/*.o

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
install: lib $(PACKAGE-CONF)
	mkdir -p $(INSTALL-DIR-PACKAGE-CONF)
	mkdir -p $(INSTALL-DIR-BINARIES)
	mkdir -p $(INSTALL-DIR-INTERFACE)
	cd $(BUILD-DIR); find -name "*.*hi" | xargs tar cf - | tar x -C $(INSTALL-DIR-INTERFACE)
	cp $(LIB-DIR)/*.so $(INSTALL-DIR-BINARIES)
	cp $(BUILD-DIR)/*.a $(INSTALL-DIR-INTERFACE)
	cp $(GEN-DIR)/$(PACKAGE-NAME).conf $(INSTALL-DIR-PACKAGE-CONF)/$(PACKAGE-NAME).conf

install-clean:
	rm -r $(INSTALL-PATH)/*
