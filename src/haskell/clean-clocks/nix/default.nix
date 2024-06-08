{
  haskell-pkg,
  lens,
  parselib,
  perlude,
  boollib,
}:
haskell-pkg {
  name = "clean-clocks";
  src = ./../src;
  haskell-libs = [
    lens
    parselib
    perlude
    boollib
  ];
}
