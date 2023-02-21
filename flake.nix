{
  description = "Sam's monorepo derivation collection";

  inputs = {
    nixpkgs-upstream.url = github:NixOS/nixpkgs;
    nixpkgs-stable.url = github:NixOS/nixpkgs/nixos-22.11;
    nixpkgs-sam.url = github:samuelrivas/nixpkgs;
  };
  outputs = { self, nixpkgs-stable, nixpkgs-upstream, nixpkgs-sam }:
    let
      supportedSystems = [ "x86_64-linux" ];
      lib = nixpkgs-stable.lib;
      forAllSystems = lib.genAttrs supportedSystems;
      pkgs-sam = nixpkgs: {
        udp-cat = with nixpkgs;
          callPackage ./nix/pkgs/applications/networking/tools/udp-cat { };
      };
    in rec {
      overlays.default = final: prev: pkgs-sam prev;
      packages = forAllSystems (system:
        let all-pkgs = pkgs-sam (import nixpkgs-stable { inherit system; });
        in all-pkgs // { default = all-pkgs.udp-cat; }
      );
      # defaultPackage = forAllSystems (system: packages.${system}.udp-cat);
    };
}
