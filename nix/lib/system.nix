{
  lib-nixpkgs,
  lib-sam,
  packages-nixpkgs,
  packages-sam,
  system,
}: {
  haskell = import ./system/haskell.nix {
    inherit lib-nixpkgs lib-sam packages-nixpkgs;
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
    inherit (packages-nixpkgs.lib) mapAttrsToList filterAttrs;
    inherit (packages-nixpkgs.lib.meta) availableOn;
    inherit system;
  };
}
