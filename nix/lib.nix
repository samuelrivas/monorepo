{lib-nixpkgs}: {
  system = import ./lib/system.nix;
  flake = import ./lib/flake.nix {nixpkgs-lib = lib-nixpkgs;};
}
