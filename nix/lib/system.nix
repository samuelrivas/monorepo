{
  lib-nixpkgs,
  packages-nixpkgs,
  packages-sam,
}: {
  haskell = import ./system/haskell.nix {
    inherit lib-nixpkgs packages-nixpkgs;
    inherit
      (packages-sam)
      my-emacs
      haskell-mk
      haskell-lib-mk
      haskell-test-mk
      ;
  };
  builders = import ./system/builders.nix {
    inherit (packages-nixpkgs) writeScript;
  };
  packages = import ./system/packages.nix {
    inherit (packages-nixpkgs) linkFarm;
    inherit (packages-nixpkgs.lib) mapAttrsToList;
  };
}
