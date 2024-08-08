{
  input-nixpkgs,
  instantiate-lib-system,
  input-vscode-extensions,
  lib-nixpkgs,
  lib-sam,
  packages-generator,
  system,
}: let
  nixpkgs = lib-sam.flake.instantiate-nixpkgs input-nixpkgs system;
  lib-system.sam = instantiate-lib-system input-nixpkgs packages system;

  packages = packages-generator {
    inherit lib-nixpkgs lib-sam lib-system nixpkgs;
    inherit
      (input-vscode-extensions.outputs.extensions.${system})
      vscode-marketplace
      ;
  };
in
  packages
