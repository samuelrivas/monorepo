{
  haskell-pkg,
  multiset,
  random-fu,
}: haskell-pkg {
  name = "photoalbum";
  src = ./../src;
  haskell-libs = [
    multiset
    random-fu
  ];
}
