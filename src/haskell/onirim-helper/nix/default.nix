{
  haskell-pkg,
  ghcWithPackages,
  generic-lens,
  lens,
  multiset,
  random-fu,
  readline,
}: haskell-pkg {
  name = "onirim-helper";
  src = ./../src;
  haskell-libs = [
    generic-lens
    lens
    multiset
    random-fu
    readline
  ];
  inherit ghcWithPackages;
}
