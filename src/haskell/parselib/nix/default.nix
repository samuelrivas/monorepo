{
  ghcWithPackages,
  haskell-lib-pkg,
  parsec,
  perlude,
  shellFor,
}:
haskell-lib-pkg {
  name = "parselib";
  src = ./../src;
  haskell-libs = [
    parsec
    perlude
  ];

  inherit shellFor ghcWithPackages;
}
