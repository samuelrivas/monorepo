{
  ghcWithPackages,
  haskell-pkg,
  lens,
  parselib,
  perlude,
  shellFor,
}: haskell-pkg {
  name = "clean-clocks";
  src = ./../src;
  haskell-libs = [
    lens
    parselib
    perlude
  ];
  inherit ghcWithPackages shellFor;
}
