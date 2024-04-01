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
    packages-2 = for-all-systems (
      system:
        import ./nix/packages.nix {
          lib = nixpkgs-stable.lib;
          system-lib = {
            sam = lib.sam.system {
              packages-nixpkgs = nixpkgs-stable.outputs.legacyPackages.${system};
              packages-sam = packages-2.${system};
            };
          };
          nixpkgs = nixpkgs-stable.outputs.legacyPackages.${system};
          vscode-extensions = vscode-extensions.outputs.extensions.${system};
        }
    );
    packages = for-all-systems (
      system: let
        pkgs-stable = instantiate-nixpkgs nixpkgs-stable system;
        pkgs-22-11 = instantiate-nixpkgs nixpkgs-22-11 system;
        bundle-pkgs-sam = pkgs:
          pkgs-stable.linkFarm "all-pkgs-sam" (
            nixpkgs-lib.mapAttrsToList
            (n: v: {
              name = n;
              path = v;
            })
            pkgs
          );
        pkgs-sam =
          pkgs-stable.derivations-sam
          // {
            # These don't build with nixpkgs-stable. We will be eventually fix
            # them to avoid carrying old versions of nixpkgs around
            adventofcode-2019 = pkgs-22-11.derivations-sam.adventofcode-2019;
          };

        # This is a derivation that installs all our derivations under a
        # single directory as a link farm. Useful mainly to build all of the
        # for testing purposes. e.g with nix build .#all-pkgs-sam
        all-pkgs-sam = bundle-pkgs-sam pkgs-sam;
      in
        pkgs-sam
        // {
          inherit all-pkgs-sam;
          default = all-pkgs-sam;
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
