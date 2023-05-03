{
  description = "Sam's monorepo derivation collection";

  inputs = {
    # nixpkgs-upstream.url = github:NixOS/nixpkgs;
    nixpkgs-stable.url = github:NixOS/nixpkgs/nixos-22.11;
    # nixpkgs-sam.url = github:samuelrivas/nixpkgs;
  };
  outputs = {
    self,
    nixpkgs-stable,
    # nixpkgs-upstream,
    # nixpkgs-sam
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
        in pkgs-stable.derivations-sam
           // { default = pkgs-stable.derivations-sam.all-pkgs-sam; }
      );
    };
}
