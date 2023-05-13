{
  ghcWithPackages,
  haskell-lib-pkg,
  parsec,
  perlude,
}:
haskell-lib-pkg {
  name = "parselib";
  src = ./../src;
  haskell-libs = [
    parsec
    perlude
  ];

  inherit ghcWithPackages;
}
