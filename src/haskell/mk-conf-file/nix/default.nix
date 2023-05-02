{
  optparse-applicative,
  ghcWithPackages,
  haskell-pkg,
  shellFor
}: haskell-pkg {
  name = "mk-conf-file";
  src = ./../src;
  haskell-libs = [
    optparse-applicative
  ];
  inherit shellFor ghcWithPackages;
}
