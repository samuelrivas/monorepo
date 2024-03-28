{pkgs}: {
  haskell = import ./system-lib/haskell.nix {
    inherit pkgs;
  };
  builders = import ./system-lib/builders.nix {
    inherit (pkgs) writeScript;
  };
}
