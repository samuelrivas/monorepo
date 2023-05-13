{
  ghcWithPackages,
  haskell-pkg,
  lens,
  parselib,
  perlude,
}: haskell-pkg {
  name = "clean-clocks";
  src = ./../src;
  haskell-libs = [
    lens
    parselib
    perlude
  ];
  inherit ghcWithPackages;
}
