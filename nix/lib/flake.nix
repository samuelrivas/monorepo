{
  # lib from nixpkgs
  nixpkgs-lib,
}: {
  # nixpkgs flakes outputs are already configured, so unfree packages are not
  # directly accessible without running `nix` with `--impure`. We instantiate
  # nixpkgs configured with access to unfree to avoid that
  instantiate-nixpkgs = supported-systems: nixpkgs-version: system:
    import nixpkgs-version {
      inherit system;
      config = {
        allowUnfree = true;
      };
    };
  for-all-systems = nixpkgs-lib.genAttrs;
}
