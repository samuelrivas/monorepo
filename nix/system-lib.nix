{pkgs}: {
  haskell = import ./system-lib/haskell.nix {inherit pkgs;};
}
