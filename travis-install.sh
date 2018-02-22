#!/usr/bin/env bash

set -euo pipefail

export NIX_CURL_FLAGS=-sS

readonly PKGS_DIR="/tmp/upstream-pkgs"
readonly PKGS_SHA="9553ca5bb6c01bb72421eb85932feb753c3ec3de"

install_nix() {
    curl -sS https://nixos.org/nix/install > install-nix
    bash ./install-nix
}

create_config() {
    local config_dir="$HOME/.local-nix-config"
    mkdir "$config_dir"
    cat > "$config_dir/configuration.nix"  <<EOF
{
  emacs-config = {
    org-agenda-file = /dev/null;
    org-template-dir = /dev/null;
    org-meeting-file = /dev/null;
    org-interview-file = /dev/null;
    org-todo-file = /dev/null;
  };
  upstream-pkgs = {
    dir = $PKGS_DIR/nixpkgs;
  };
  sams-pkgs = {
    dir = "$PWD/nix";
  };
}
EOF
}

install_upstream_nixpkgs() {
    mkdir $PKGS_DIR
    curl -sSL "https://github.com/NixOS/nixpkgs/archive/$PKGS_SHA.zip" \
         > $PKGS_DIR/nixpkgs.zip
    cd $PKGS_DIR
    unzip nixpkgs.zip > /dev/null
    mv "nixpkgs-$PKGS_SHA" nixpkgs
}

reset_channel() {
    nix-channel --remove nixpkgs
    nix-channel --add https://nixos.org/channels/nixos-17.03 nixpkgs
    nix-channel --update
}

main() {
    install_nix

    source "$HOME/.nix-profile/etc/profile.d/nix.sh"

    reset_channel

    install_upstream_nixpkgs
    create_config
}

main
