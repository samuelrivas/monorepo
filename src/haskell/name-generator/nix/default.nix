{
  haskell-pkg,
  random-fu,
}:
haskell-pkg {
  name = "name-generator";
  src = ./../src;
  haskell-libs = [
    random-fu
  ];
}
