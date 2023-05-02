{
  ghcWithPackages,
  haskell-pkg,
  random-fu,
  shellFor,
}: haskell-pkg {
  name = "boardgamer";
  src = ./../src;
  haskell-libs = [
    random-fu
  ];
  inherit ghcWithPackages shellFor;
}
