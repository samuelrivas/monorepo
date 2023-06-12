{
  description = "Sam's monorepo derivation collection";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-22.11;
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs =
    inputs@{ flake-parts, nixpkgs, self }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      # See https://flake.parts/options/flake-parts.html#opt-debug
      debug = true;
      systems = [ "x86_64-linux" ];
      flake = { };
      _module.args.sam-lib = self.lib;
      imports = [
        ./nix/pkgs-sam.nix
        ./nix/lib.nix
      ];
      perSystem = { config, ... }: {
      };
    };
}
