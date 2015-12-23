#!/usr/bin/env bash

set -e
set -u

export NIX_CURL_FLAGS=-sS

source "$HOME/.nix-profile/etc/profile.d/nix.sh"

info() {
    echo "NIX_PATH=$NIX_PATH"
    echo "Channels Root"
    ls -lh ~/.nix-defexpr/channels/nixpkgs
}

build_all() {
    nix-build nix
}

main() {
    info
    build_all
}

main
