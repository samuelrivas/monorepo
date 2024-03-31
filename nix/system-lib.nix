{
  packages-nixpkgs,
  packages-sam,
}: {
  haskell = import ./system-lib/haskell.nix {
    inherit packages-nixpkgs packages-sam;
  };
  builders = import ./system-lib/builders.nix {
    inherit (packages-nixpkgs) writeScript;
  };
}
