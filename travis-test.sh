#!/usr/bin/env bash

set -euo pipefail

export NIX_CURL_FLAGS=-sS

source "$HOME/.nix-profile/etc/profile.d/nix.sh"

info() {
    echo "NIX_PATH=$NIX_PATH"
}

build_all() {
    nix-build -A pkgs-sam nix
}

main() {
    info
    build_all
}

main
