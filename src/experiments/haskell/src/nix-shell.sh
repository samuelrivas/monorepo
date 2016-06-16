#!/usr/bin/env bash

set -eu

SCRIPT_HOME="$( cd "$( dirname "$0" )" && pwd )"
NIX_GC_ROOTS_DIR="$SCRIPT_HOME/../build/nix-gc-roots"
NIX_GC_ROOTS="$NIX_GC_ROOTS_DIR/gc-root"
NIX_DIR="$SCRIPT_HOME/../../../../nix"
NIX_DERIVATION="with import $NIX_DIR {}; experiments-haskell.override {sandbox = true;}"
NIX_EXPRESSIONS="$NIX_DIR"

mkdir -p "$NIX_GC_ROOTS_DIR"

nix-shell				  \
    --pure				  \
    --indirect --add-root "$NIX_GC_ROOTS" \
    -E "$NIX_DERIVATION"		  \
    "$NIX_EXPRESSIONS"
