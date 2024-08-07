{lib-nixpkgs}: {
  derivation-helpers =
    import ./lib/derivation-helpers.nix {inherit lib-nixpkgs;};
  system = import ./lib/system.nix;
  flake = import ./lib/flake.nix {inherit lib-nixpkgs;};
}
