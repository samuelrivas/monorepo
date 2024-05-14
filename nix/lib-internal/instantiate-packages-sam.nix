{
  input-nixpkgs,
  input-vscode-extensions,
  legacy-lib,
  lib-nixpkgs,
  lib-sam,
  packages-file,
  system,
}: let
  nixpkgs = lib-sam.flake.instantiate-nixpkgs input-nixpkgs system;
  lib-system.sam = lib-sam.system {
    inherit lib-nixpkgs;
    packages-nixpkgs = nixpkgs;
    packages-sam = packages;
  };

  packages = import packages-file {
    inherit legacy-lib lib-nixpkgs nixpkgs lib-system;
    inherit (input-vscode-extensions.outputs.extensions.${system}) vscode-marketplace;
  };
in
  packages
