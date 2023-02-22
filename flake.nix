{
  description = "Sam's monorepo derivation collection";

  inputs = {
    nixpkgs-upstream.url = github:NixOS/nixpkgs;
    nixpkgs-stable.url = github:NixOS/nixpkgs/nixos-22.11;
    nixpkgs-sam.url = github:samuelrivas/nixpkgs;
  };
  outputs = { self, nixpkgs-stable, nixpkgs-upstream, nixpkgs-sam }:
    let
      supported-systems = [ "x86_64-linux" ];
      lib = nixpkgs-stable.lib;
      for-all-systems = lib.genAttrs supported-systems;
      # pkgs-sam = nixpkgs: {
      #   udp-cat = with nixpkgs;
      #     callPackage ./nix/pkgs/applications/networking/tools/udp-cat { };
      # };
      pkgs-sam = import ./nix/pkgs-sam.nix;
    in rec {
      overlays.default = final: prev: (prev // pkgs-sam prev);

      # TODO: use the overlay here, but you will need to have legacyPackages to
      # avoid cluttering this with all nixpkgs
      packages = for-all-systems (system:
        let all-pkgs = pkgs-sam (import nixpkgs-stable { inherit system; });
        in all-pkgs // { default = all-pkgs.udp-cat; }
      );
    };
}
