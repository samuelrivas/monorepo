{
  description = "Sam's monorepo derivation collection";

  inputs = {
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-23.11";
    nixpkgs-22-11.url = "github:NixOS/nixpkgs/nixos-22.11";
    vscode-extensions.url = "github:nix-community/nix-vscode-extensions/master";
  };
  outputs = {
    self,
    nixpkgs-22-11,
    nixpkgs-stable,
    vscode-extensions,
  }: let
    nixpkgs-lib = nixpkgs-stable.lib;
    sam-lib = import ./nix/lib.nix;
    sam-lib-flake = sam-lib.flake {
      inherit nixpkgs-lib;
      supported-systems = ["x86_64-linux"];
    };
  in rec {
    formatter =
      sam-lib-flake.for-all-systems (system:
        nixpkgs-stable.legacyPackages.${system}.alejandra);

    legacy.lib.sam = import ./nix/legacy/lib.nix;

    lib.sam = import ./nix/lib.nix;

    # TODO Go over these input parameters and make sense of them according to The Principles
    packages = sam-lib-flake.for-all-systems (
      system: let
        bundle-packages = p:
          nixpkgs-stable.outputs.legacyPackages.${system}.linkFarm "all-packages" (
            nixpkgs-lib.mapAttrsToList
            (n: v: {
              name = n;
              path = v;
            })
            p
          );
        instantiate-packages-sam = nixpkgs:
          import ./nix/packages.nix {
            legacy-lib = legacy.lib.sam;
            lib = nixpkgs.lib;
            system-lib = {
              sam = lib.sam.system {
                packages-nixpkgs = nixpkgs;
                packages-sam = packages.${system};
              };
            };
            inherit nixpkgs;
            vscode-extensions = vscode-extensions.outputs.extensions.${system};
          };
        packages-sam-stable = instantiate-packages-sam (sam-lib-flake.instantiate-nixpkgs nixpkgs-stable system);
        packages-sam-22-11 = instantiate-packages-sam (sam-lib-flake.instantiate-nixpkgs nixpkgs-22-11 system);
        final-packages =
          packages-sam-stable
          // {
            # These don't build with nixpkgs-stable. We will be eventually fix
            # them to avoid carrying old versions of nixpkgs around
            adventofcode-2019 = packages-sam-22-11.adventofcode-2019;
          };
        all-packages = bundle-packages final-packages;
      in
        final-packages
        // {
          inherit all-packages;
          default = all-packages;
        }
    );

    devShells = sam-lib-flake.for-all-systems (
      system:
        builtins.mapAttrs
        (name: value:
          if builtins.hasAttr "dev-shell" packages.${system}.${name}
          then packages.${system}.${name}.dev-shell
          else packages.${system}.${name})
        packages.${system}
    );
  };
}
