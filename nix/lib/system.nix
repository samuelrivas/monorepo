{
  packages-nixpkgs,
  packages-sam,
}: {
  haskell = import ./system/haskell.nix {
    inherit packages-nixpkgs packages-sam;
  };
  builders = import ./system/builders.nix {
    inherit (packages-nixpkgs) writeScript;
  };
}
