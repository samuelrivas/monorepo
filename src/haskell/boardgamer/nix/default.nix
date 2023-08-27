{
  haskell-pkg,
  random-fu,
}:
haskell-pkg {
  name = "boardgamer";
  src = ./../src;
  haskell-libs = [
    random-fu
  ];
}
