# Interface
# =========

# PREFIX must be set when installing

PACKAGE-NAME := adventlib

# Internal variables
# ==================

BUILD-DIR := $(realpath ..)/build
LIB-DIR := $(BUILD-DIR)/lib
BIN-DIR := $(BUILD-DIR)/bin
GEN-DIR := $(BUILD-DIR)/generated

# TODO: Many of these must be generalised when improving this Makefile
GHC-VERSION := 8.6.5
ARCH = x86_64-linux
LIB-NAME = adventlib
LIB-VERSION = 1.0

INSTALL-DIR := $(PREFIX)/lib/ghc-$(GHC-VERSION)
INSTALL-DIR-PACKAGE-CONF := $(INSTALL-DIR)/package.conf.d
INSTALL-DIR-BINARIES := $(INSTALL-DIR)/$(ARCH)-ghc-$(GHC-VERSION)
INSTALL-DIR-INTERFACE := $(INSTALL-DIR-BINARIES)/$(LIB-NAME)-$(LIB-VERSION)

# Targets
# =======
$(LIB-DIR):
	mkdir -p $@

$(BIN-DIR):
	mkdir -p $@

$(GEN-DIR):
	mkdir -p $@

$(GEN-DIR)/$(PACKAGE-NAME).conf: templates/$(PACKAGE-NAME).conf.template | $(GEN-DIR)
	sed -e "s|@@INSTALL-DIR-INTERFACE@@|$(INSTALL-DIR-INTERFACE)|; \
		s|@@INSTALL-DIR-BINARIES@@|$(INSTALL-DIR-BINARIES)|" \
	< $< > $@

.PHONY: all
all: $(GEN-DIR)/$(PACKAGE-NAME).conf lib

.PHONY: lib
lib: | $(LIB-DIR)
	cd src; ghc -outputdir $(BUILD-DIR) --make -dynamic -shared -fPIC -package-name adventlib System/IO/Advent.hs System/IO/Test.hs -osuf dyn_o -hisuf dyn_hi -o libHSadventlib-ghc8.6.5.so
	cd src; ghc -c --make -outputdir $(BUILD-DIR) -package-name adventlib System/IO/Advent.hs System/IO/Test.hs
	mv src/*.so $(LIB-DIR)
	ar cqs $(BUILD-DIR)/libHSadventlib.a $(BUILD-DIR)/System/IO/*.o

.PHONY: test
test:
	# ghc --make src/MainTest.hs
	# ghc --make -dynamic src/MainTest.hs -o src/MainTest_dyn
	# src/MainTest
	# src/MainTest_dyn

clean:
	rm -rf $(BUILD-DIR)
	find . -name "*.hi" -delete
	find . -name "*.o" -delete
	find . -name "*.a" -delete
	find . -name "*dyn*" -delete
	find . -name "*.hi" -delete
	find . -name "*.so" -delete
	rm -f src/MainTest


.PHONY: install-good
install-good:
ifeq ($(PREFIX),)
	$(error "PREFIX is not set, please set it with make PREFIX=<path>")
endif
	mkdir -p $(PREFIX);
	cp -r 

.PHONY: install
install:
ifeq ($(PREFIX),)
	$(error "PREFIX is not set, please set it with make PREFIX=<path>")
endif

	mkdir -p $(INSTALL-DIR-PACKAGE-CONF)
	mkdir -p $(INSTALL-DIR-BINARIES)
	mkdir -p $(INSTALL-DIR-INTERFACE)
	cd $(BUILD-DIR); find -name "*.*hi" | xargs tar cf - | tar x -C $(INSTALL-DIR-INTERFACE)
	cp $(LIB-DIR)/*.so $(INSTALL-DIR-BINARIES)
	cp $(BUILD-DIR)/*.a $(INSTALL-DIR-INTERFACE)
	cp $(GEN-DIR)/$(PACKAGE-NAME).conf $(INSTALL-DIR-PACKAGE-CONF)/$(PACKAGE-NAME).conf

install-clean:
	rm -r $(INSTALL-PATH)/*
