#!/usr/bin/env bash

set -euo pipefail

export NIX_CURL_FLAGS=-sS

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
    full-user-name = "Test user";
    blacklisted-modes = [ ];
    extra-config = "";
  };
  sams-pkgs = {
    dir = "$PWD/nix";
  };
}
EOF
}

main() {
    install_nix
    create_config
}

main
