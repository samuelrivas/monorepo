{
  # lib from nixpkgs
  lib-nixpkgs,
}: let

  # You can use this in the flake to create a "for-all-supported-systems" passing
  # list of supported systems.
  for-all-systems = lib-nixpkgs.genAttrs;

  # Generates a dev shell for a package.
  #
  # By convention, we put an atrribute `dev-shell` in the package that depends
  # on the derivations that we want to have when developing. This is alternative
  # to, for example numtide's devshell. When a package contains a dev-shell, we
  # use that for the flake devShell output, otherwise we pass the package itself.
  #
  # TODO: move this to use passthru
  make-dev-shell = package:
    if builtins.hasAttr "dev-shell" package
    then package.dev-shell
    else package;
in {
  inherit make-dev-shell for-all-systems;

  # nixpkgs flakes outputs are already configured, so unfree packages are not
  # directly accessible without running `nix` with `--impure`. We instantiate
  # nixpkgs configured with access to unfree to avoid that
  #
  # nxipkgs-root-expression: the root expression of nixpkgs, you can directly
  # pass the nixpkgs input from a flake, for example
  instantiate-nixpkgs = nixpkgs-root-expression: system:
    import nixpkgs-root-expression {
      inherit system;
      config = {
        allowUnfree = true;
      };
    };

  # Takes the packages output, which is a set of systems, and returns the same
  # set after applying make-dev-shell to all packages in each system
  make-dev-shells = output-packages:
    builtins.mapAttrs
    (
      system: packages:
        builtins.mapAttrs
        (name: package: make-dev-shell package)
        packages
    )
    output-packages;
}
