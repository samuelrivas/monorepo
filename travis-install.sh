#!/usr/bin/env bash

set -e
set -u

export NIX_CURL_FLAGS=-sS

install_nix() {
    curl -sS https://nixos.org/nix/install > install-nix
    bash ./install-nix
}

reset_channel() {
    nix-channel --remove nixpkgs
    nix-channel --add https://nixos.org/channels/nixos-15.09 nixpkgs
    nix-channel --update
}

main() {
    install_nix

    source "$HOME/.nix-profile/etc/profile.d/nix.sh"

    reset_channel
}

main
