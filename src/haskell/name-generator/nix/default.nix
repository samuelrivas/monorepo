{
  ghcWithPackages,
  haskell-pkg,
  random-fu,
  shellFor,
}: haskell-pkg {
  name = "name-generator";
  src = ./../src;
  haskell-libs = [
    random-fu
  ];
  inherit ghcWithPackages shellFor;
}
