{
  description = "Sam's monorepo derivation collection";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-22.11;
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs =
    inputs@{
      flake-parts,
      nixpkgs,
      self,
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      flake = let
        supported-systems = [ "x86_64-linux" ];
        lib = nixpkgs.lib;
        for-all-systems = lib.genAttrs supported-systems;
      in rec {
        overlays.default = import ./nix/pkgs-sam.nix;

        # TODO: use the overlay here, but you will need to have legacyPackages to
        # avoid cluttering this with all nixpkgs
        packages = for-all-systems (system:
          let
            pkgs-stable = import nixpkgs {
              inherit system;
              overlays = [ self.overlays.default ];
              config = { };
            };
          in pkgs-stable.derivations-sam
             // { default = pkgs-stable.derivations-sam.all-pkgs-sam; }
        );

        devShells = for-all-systems (system:
          builtins.mapAttrs
            (name: value:
              if builtins.hasAttr "dev-shell" packages.${system}.${name}
              then packages.${system}.${name}.dev-shell
              else packages.${system}.${name})
            packages.${system}
        );
      };
      systems = [ "x86_64-linux" ];
      perSystem = { config, system, lib, specialArgs, options } : { };
    };
}
