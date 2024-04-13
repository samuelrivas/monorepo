{
  # lib from nixpkgs
  nixpkgs-lib,
}: {
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
  for-all-systems = nixpkgs-lib.genAttrs;
}
