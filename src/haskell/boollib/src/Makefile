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

# FIXME: finding the haddock file with a star is not correct, though it works in practice, what we want is the haddock-interfaces field
HADDOCK-HTML-DIRS := $(shell ghc-pkg field '*' haddock-html --simple-output)
HADDOCK-INTERFACE-FLAGS := $(foreach dir,$(HADDOCK-HTML-DIRS),-i file://$(dir),file://$(dir)/src,$(wildcard $(dir)/*.haddock))


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

.PHONY: install-doc
install-doc: doc | $(INSTALL-DOC-DIR)
	cp -r $(BUILD-DOC-DIR)/* $(INSTALL-DOC-DIR)

$(INSTALL-DOC-DIR) \
$(BUILD-DOC-DIR):
	mkdir -p $@
