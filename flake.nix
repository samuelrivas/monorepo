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
    legacy-lib = import ./nix/legacy/lib.nix;

    supported-systems = ["x86_64-linux"];
    for-all-supported-systems = lib-sam.flake.for-all-systems supported-systems;

    inherit (lib-sam.flake) instantiate-nixpkgs;

    instantiate-lib-system = input-nixpkgs: packages-sam: system:
      lib-sam.system {
        inherit lib-nixpkgs;
        packages-nixpkgs = instantiate-nixpkgs input-nixpkgs system;
        packages-sam = packages-sam.${system};
      };

    outputs = rec {
      formatter =
        for-all-supported-systems (system:
          nixpkgs-stable.legacyPackages.${system}.alejandra);

      legacy.lib.sam = legacy-lib;

      lib.sam = lib-sam;

      packages = for-all-supported-systems (
        system: let
          lib-system = instantiate-lib-system nixpkgs-stable packages system;

          bundle-packages = lib-system.packages.bundle {name = "all-packages";};
          instantiate-packages-sam = input-nixpkgs:
            import ./nix/packages.nix {
              inherit legacy-lib lib-nixpkgs;
              lib-system = {
                sam = instantiate-lib-system input-nixpkgs packages system;
              };
              nixpkgs = instantiate-nixpkgs input-nixpkgs system;
              vscode-extensions = vscode-extensions.outputs.extensions.${system};
            };
          packages-sam-stable = instantiate-packages-sam nixpkgs-stable;
          packages-sam-22-11 = instantiate-packages-sam nixpkgs-22-11;
          packages-final =
            packages-sam-stable
            // {
              # These don't build with nixpkgs-stable. We will be eventually fix
              # them to avoid carrying old versions of nixpkgs around
              adventofcode-2019 = packages-sam-22-11.adventofcode-2019;
            };
          # If something is failing, you can temporarily remove packages from this
          # list by adding to the removeAttrs list below
          all-packages = bundle-packages (builtins.removeAttrs packages-final []);
        in
          packages-final
          // {
            inherit all-packages;
            default = all-packages;
          }
      );

      devShells = lib-sam.flake.make-dev-shells packages;
    };
  in
    outputs;
}
