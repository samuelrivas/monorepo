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
    lib-nixpkgs = nixpkgs-stable.lib;
    lib-sam = import ./nix/lib.nix {
      inherit lib-nixpkgs;
    };

    supported-systems = ["x86_64-linux"];
    for-all-systems = lib-sam.flake.for-all-systems supported-systems;

    inherit (lib-sam.flake) instantiate-nixpkgs;

    instantiate-lib-system = input-nixpkgs: packages-sam: system:
      lib-sam.system {
        packages-nixpkgs = instantiate-nixpkgs input-nixpkgs system;
        packages-sam = packages-sam.${system};
      };

    outputs = rec {
      formatter =
        for-all-systems (system:
          nixpkgs-stable.legacyPackages.${system}.alejandra);

      legacy.lib.sam = import ./nix/legacy/lib.nix;

      lib.sam = lib-sam;

      # TODO Go over these input parameters and make sense of them according to The Principles
      packages = for-all-systems (
        system: let
          lib-system = instantiate-lib-system nixpkgs-stable packages system;

          bundle-packages = lib-system.packages.bundle {name = "all-packages";};
          instantiate-packages-sam = input-nixpkgs:
            import ./nix/packages.nix {
              legacy-lib = legacy.lib.sam;
              lib-nixpkgs = nixpkgs-stable.outputs.lib;
              lib-system = {
                sam = instantiate-lib-system input-nixpkgs packages system;
              };
              nixpkgs = instantiate-nixpkgs input-nixpkgs system;
              vscode-extensions = vscode-extensions.outputs.extensions.${system};
            };
          packages-sam-stable = instantiate-packages-sam nixpkgs-stable; # (instantiate-nixpkgs nixpkgs-stable system);
          packages-sam-22-11 = instantiate-packages-sam nixpkgs-22-11; # (instantiate-nixpkgs nixpkgs-22-11 system);
          final-packages =
            packages-sam-stable
            // {
              # These don't build with nixpkgs-stable. We will be eventually fix
              # them to avoid carrying old versions of nixpkgs around
              adventofcode-2019 = packages-sam-22-11.adventofcode-2019;
            };
          # TODO Fix whatever broke in 2019
          all-packages = bundle-packages (builtins.removeAttrs final-packages []);#["adventofcode-2019"]);
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
  in
    outputs;
}
