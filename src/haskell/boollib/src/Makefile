PACKAGE-NAME := boollib
PACKAGE-VERSION := 1.0
PACKAGE-DEPS := base
EXPOSED-MODULES := Data.Bool.Boollib
include $(HASKELL_LIB_MK)

DOC-RELATIVE-DIR := $(PACKAGE-NAME)/html
DOC-DIR := $(BUILD-DIR)/doc
DOC-INSTALL-DIR ?= $(PREFIX)/doc/

BUILD-DOC-DIR := $(DOC-DIR)/$(DOC-RELATIVE-DIR)
INSTALL-DOC-DIR := $(DOC-INSTALL-DIR)/$(DOC-RELATIVE-DIR)

HADDOCK-DIR := /nix/store/2pd2zd2qzlmsd95b0ix12s8180d2c4z1-ghc-9.0.2-doc/share/doc/ghc/html/libraries/ghc-9.0.2

.PHONY: docs
doc: | $(BUILD-DOC-DIR)
	haddock \
	--html \
	--hoogle \
	--hyperlinked-source \
	--odir $(BUILD-DOC-DIR) \
	--package-name $(PACKAGE-NAME) \
	--quickjump \
	-i file://$(HADDOCK-DIR),file://$(HADDOCK-DIR)/src,$(HADDOCK-DIR)/ghc.haddock \
	--dump-interface=$(BUILD-DOC-DIR)/$(PACKAGE-NAME).haddock \
	$(SRCS)

.PHONY: install-doc
install-doc: doc | $(INSTALL-DOC-DIR)
	cp -r $(BUILD-DOC-DIR)/* $(INSTALL-DOC-DIR)

$(INSTALL-DOC-DIR) \
$(BUILD-DOC-DIR):
	mkdir -p $@
