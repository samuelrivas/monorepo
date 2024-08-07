{
  input-nixpkgs,
  input-vscode-extensions,
  lib-nixpkgs,
  lib-sam,
  packages-generator,
  system,
}: let
  nixpkgs = lib-sam.flake.instantiate-nixpkgs input-nixpkgs system;
  lib-system.sam = lib-sam.system {
    inherit lib-nixpkgs system;
    packages-nixpkgs = nixpkgs;
    packages-sam = packages;
  };

  packages = packages-generator {
    inherit lib-nixpkgs lib-sam lib-system nixpkgs;
    inherit
      (input-vscode-extensions.outputs.extensions.${system})
      vscode-marketplace
      ;
  };
in
  packages
