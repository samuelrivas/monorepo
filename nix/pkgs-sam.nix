nixpkgs:
{
  udp-cat = with nixpkgs;
    callPackage ./nix/pkgs/applications/networking/tools/udp-cat { };
};

