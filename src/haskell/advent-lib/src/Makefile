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
PACKAGE-DIR := $(PREFIX)/lib/ghc-$(GHC-VERSION)

PACKAGE-CONF-DIR := $(PACKAGE-DIR)/package.conf.d
DYNAMIC-LIB-DIR := $(PACKAGE-DIR)/$(ARCH)-ghc-$(GHC-VERSION)
STATIC-LIB-DIR := $(PACKAGE-DIR)/$(ARCH)-ghc-$(GHC-VERSION)/$(PACKAGE-NAME)-$(PACKAGE-VERSION)

PACKAGE-CONF := $(PACKAGE-CONF-DIR)/$(PACKAGE-NAME).conf

# Targets
# =======
$(PACKAGE-DIR) \
$(PACKAGE-CONF-DIR) \
$(DYNAMIC-LIB-DIR) \
$(STATIC-LIB-DIR):
	mkdir -p $@

.PHONY: all
all: lib $(PACKAGE-CONF)

$(PACKAGE-CONF): templates/$(PACKAGE-NAME).conf.template | $(PACKAGE-CONF-DIR) $(DYNAMIC-LIB-DIR) $(STATIC-LIB-DIR)
	sed -e "s|@@INSTALL-DIR-INTERFACE@@|$(realpath $(STATIC-LIB-DIR))|; \
		s|@@INSTALL-DIR-BINARIES@@|$(realpath $(DYNAMIC-LIB-DIR))|" \
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
