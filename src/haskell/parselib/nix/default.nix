{
  haskell-lib-pkg,
  parsec,
  perlude,
}:
haskell-lib-pkg {
  name = "parselib";
  src = ./../src;
  build-doc = true;
  haskell-libs = [
    parsec
    perlude
  ];
}
