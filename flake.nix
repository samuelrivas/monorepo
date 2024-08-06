{
  description = "Sam's monorepo derivation collection";

  inputs = {
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-22-11.url = "github:NixOS/nixpkgs/nixos-22.11";
    vscode-extensions.url = "github:nix-community/nix-vscode-extensions/master";
  };
  outputs = {
    self,
    nixpkgs-22-11,
    nixpkgs-unstable,
    vscode-extensions,
  }: let
    lib-nixpkgs = nixpkgs-unstable.lib;
    lib-sam = import ./nix/lib.nix {
      inherit lib-nixpkgs;
    };
    legacy-lib = import ./nix/legacy/lib.nix;

    supported-systems = ["x86_64-linux" "aarch64-darwin"];
    for-all-supported-systems = lib-sam.flake.for-all-systems supported-systems;

    inherit (lib-sam.flake) instantiate-nixpkgs;

    instantiate-lib-system = input-nixpkgs: packages-sam: system:
      lib-sam.system {
        inherit lib-nixpkgs system;
        packages-nixpkgs = instantiate-nixpkgs input-nixpkgs system;
        packages-sam = packages-sam.${system};
      };

    # We parametereize on the nixpkgs input so that we can build packages that
    # are broken in nixpkgs-stable until we fix them
    instantiate-packages-sam = input-nixpkgs: system:
      import ./nix/lib-internal/instantiate-packages-sam.nix {
        inherit legacy-lib lib-sam lib-nixpkgs system input-nixpkgs;
        input-vscode-extensions = vscode-extensions;
        packages-generator = import ./nix/packages.nix;
      };

    outputs = rec {
      formatter =
        for-all-supported-systems (system:
          nixpkgs-unstable.legacyPackages.${system}.alejandra);

      legacy.lib.sam = legacy-lib;

      lib.sam = lib-sam;

      packages = for-all-supported-systems (
        system: let
          lib-system = instantiate-lib-system nixpkgs-unstable packages system;
          bundle-packages = lib-system.packages.bundle {name = "all-packages";};

          # Some packages are broken with nixpkgs-stable, so instantiate them
          # with older for now
          packages-sam-stable = instantiate-packages-sam nixpkgs-unstable system;
          packages-sam-22-11 = instantiate-packages-sam nixpkgs-22-11 system;
          packages-final =
            packages-sam-stable
            // {
              finndb = packages-sam-22-11.finndb;
            };

          # all-packages is a derivation that builds all packages in the monorepo
          #
          # If something is failing, you can temporarily remove packages from this
          # list by adding to the removeAttrs list below
          all-packages =
            bundle-packages (builtins.removeAttrs packages-final [
              ]);
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
