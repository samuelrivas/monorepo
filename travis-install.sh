#!/usr/bin/env bash

set -euo pipefail

create_config() {
    local config_dir="$HOME/.local-nix-config"
    mkdir "$config_dir"
    cat > "$config_dir/configuration.nix"  <<EOF
{
  emacs-config = {
    full-user-name = "Test user";
    blacklisted-modes = [ "ocaml" "erlang" ];
    extra-config = "";
  };
  sams-pkgs = {
    dir = "$PWD/nix";
  };
}
EOF
}

main() {
    create_config
}

main
