{nixpkgs-lib}: {
  system = import ./lib/system.nix;
  flake = import ./lib/flake.nix {inherit nixpkgs-lib;};
}
