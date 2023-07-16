{
  description = "Sam's monorepo derivation collection";

  inputs = {
    nixpkgs-stable.url = github:NixOS/nixpkgs/nixos-23.05;
    nixpkgs-old.url = github:NixOS/nixpkgs/nixos-22.11;
  };
  outputs = {
    self,
    nixpkgs-old,
    nixpkgs-stable,
  }:
    let
      supported-systems = [ "x86_64-linux" ];
      lib = nixpkgs-stable.lib;
      for-all-systems = lib.genAttrs supported-systems;
    in rec {
      overlays.default = import ./nix/pkgs-sam.nix;

      # TODO: use the overlay here, but you will need to have legacyPackages to
      # avoid cluttering this with all nixpkgs
      packages = for-all-systems (system:
        let
          pkgs-stable = import nixpkgs-stable {
            inherit system;
            overlays = [ self.overlays.default ];
            config = { };
          };
          pkgs-old = import nixpkgs-old {
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
}
