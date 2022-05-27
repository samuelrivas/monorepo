# Common test target. This is meant to be used with build-haskell.mk or
# build-haskell-lib.mk
#
# Heavily based on our own conventions. Namely
#   - Code is in src (from where Makefile is)
#   - All files are generated in a separate directory (BUILD-DIR)
#   - All source files are in the src dir
#   - One executable per project
#   - No cabal, we compile and link with ghc, like the good old times :P
#   - Control from the outside via PROGRAM-NAME, GHC-FLAGS, GHC-LD-FLAGS and
#     GHC-PACKAGES

# Interface
TEST-MODULE ?= test.hs
TEST-PROGRAM-NAME ?= test

# Requirements from other .mk files
GHC-FLAGS ?= $(error "GHC-FLAGS is missing include build-haskell.mk or build-haskell-lib.mk before test-haskell.mk")
BUILD-DIR ?= $(error "BUILD-DIR is missing include build-haskell.mk or build-haskell-lib.mk before test-haskell.mk")

# Internal variables
TEST-DIR := $(BUILD-DIR)/test

TEST-PROGRAM := $(TEST-DIR)/$(TEST-PROGRAM-NAME)

build-test: $(TEST-PROGRAM)

$(TEST-DIR):
	mkdir -p $@

.PHONY: $(TEST-PROGRAM)
$(TEST-PROGRAM): | $(TEST-DIR)
	cd src; ghc $(GHC-FLAGS) $(TEST-MODULE) -o ../$@

.PHONY: test
test: $(TEST-PROGRAM)
	$(TEST-PROGRAM)
