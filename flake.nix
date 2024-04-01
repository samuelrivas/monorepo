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
    supported-systems = ["x86_64-linux"];
    nixpkgs-lib = nixpkgs-stable.lib;
    instantiate-nixpkgs = nixpkgs-version: system:
      import nixpkgs-version {
        inherit system;
        overlays = [
          self.overlays.default
          vscode-extensions.overlays.default
        ];
        config = {
          allowUnfree = true;
        };
      };
    for-all-systems = nixpkgs-lib.genAttrs supported-systems;
  in rec {
    overlays.default = import ./nix/pkgs-sam.nix;
    formatter =
      for-all-systems (system:
        nixpkgs-stable.legacyPackages.${system}.alejandra);

    legacy.lib.sam = import ./nix/legacy/lib.nix;

    lib.sam = import ./nix/lib.nix;

    # TODO Go over these input parameters and make sense of them according to The Principles
    packages = for-all-systems (
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
        instantiate-packages-sam = flake-nixpkgs:
          import ./nix/packages.nix {
            lib = flake-nixpkgs.lib;
            system-lib = {
              sam = lib.sam.system {
                packages-nixpkgs = flake-nixpkgs.outputs.legacyPackages.${system};
                packages-sam = packages.${system};
              };
            };
            nixpkgs = flake-nixpkgs.outputs.legacyPackages.${system};
            vscode-extensions = vscode-extensions.outputs.extensions.${system};
          };
        packages-sam-stable = instantiate-packages-sam nixpkgs-stable;
        packages-sam-22-11 = instantiate-packages-sam nixpkgs-22-11;
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

    devShells = for-all-systems (
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
