{pkgs}: {
  haskell = import ./lib/haskell.nix {inherit pkgs;};
}
