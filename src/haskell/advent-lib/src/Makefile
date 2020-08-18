INSTALL-PATH := /please-set-install-path
GHC-VERSION := 8.6.5
ARCH = x86_64-linux
LIB-NAME = adventlib
LIB-VERSION = 1.0

INSTALL-DIR := $(INSTALL-PATH)/lib/ghc-$(GHC-VERSION)
INSTALL-DIR-PACKAGE-CONF := $(INSTALL-DIR)/package.conf.d
INSTALL-DIR-BINARIES := $(INSTALL-DIR)/$(ARCH)-ghc-$(GHC-VERSION)
INSTALL-DIR-INTERFACE := $(INSTALL-DIR-BINARIES)/$(LIB-NAME)-$(LIB-VERSION)

all: test

test:
	cd src; ghc --make -dynamic -shared -fPIC -package-name adventlib System/IO/Advent.hs System/IO/Test.hs -osuf dyn_o -hisuf dyn_hi -o libHSadventlib-ghc8.6.5.so
	cd src; ghc -c --make -package-name adventlib System/IO/Advent.hs System/IO/Test.hs
	ar cqs src/libHSadventlib.a src/System/IO/*.o
	# ghc --make src/MainTest.hs
	# ghc --make -dynamic src/MainTest.hs -o src/MainTest_dyn
	# src/MainTest
	# src/MainTest_dyn

clean:
	find . -name "*.hi" -delete
	find . -name "*.o" -delete
	find . -name "*.a" -delete
	find . -name "*dyn*" -delete
	find . -name "*.hi" -delete
	find . -name "*.so" -delete
	rm -f src/MainTest

install:
	mkdir -p $(INSTALL-DIR-PACKAGE-CONF)
	mkdir -p $(INSTALL-DIR-BINARIES)
	mkdir -p $(INSTALL-DIR-INTERFACE)
	cd src; find -name "*.*h?" | xargs tar cf - | tar x -C $(INSTALL-DIR-INTERFACE)
	# cp adventlib.conf $(INSTALL-DIR-PACKAGE-CONF)
	cp src/*.so $(INSTALL-DIR-BINARIES)
	cp src/*.a $(INSTALL-DIR-INTERFACE)
	sed -e 's|@@INSTALL-DIR-INTERFACE@@|$(INSTALL-DIR-INTERFACE)|;s|@@INSTALL-DIR-BINARIES@@|$(INSTALL-DIR-BINARIES)|' \
	< adventlib.conf.template > $(INSTALL-DIR-PACKAGE-CONF)/adventlib.conf

install-clean:
	rm -r $(INSTALL-PATH)/*
