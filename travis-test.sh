#!/usr/bin/env bash

set -euo pipefail

export NIX_CURL_FLAGS=-sS

main() {
    nix-build --max-jobs auto -A pkgs-sam nix
}

main
