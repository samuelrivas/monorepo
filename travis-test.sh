#!/usr/bin/env bash

set -euo pipefail

export NIX_CURL_FLAGS=-sS

main() {
    nix-build -A pkgs-sam nix
}

main
