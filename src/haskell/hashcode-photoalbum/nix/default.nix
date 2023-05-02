{
  ghcWithPackages,
  haskell-pkg,
  multiset,
  random-fu,
  shellFor,
}: haskell-pkg {
  name = "photoalbum";
  src = ./../src;
  haskell-libs = [
    multiset
    random-fu
  ];
  inherit ghcWithPackages shellFor;
}
