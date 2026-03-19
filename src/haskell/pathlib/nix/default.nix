{
  haskell-lib-pkg,
  hedgehog,
  parselib,
  perlude,
}:
haskell-lib-pkg {
  name = "pathlib";
  src = ./../src;
  haskell-libs = [
    hedgehog
    parselib
    perlude
  ];
}
