{
  ghcWithPackages,
  haskell-lib-pkg,
  lens,
  parselib,
  unliftio,
}:
haskell-lib-pkg {
  name = "adventlib";
  src = ./../src;
  haskell-libs = [
    lens
    parselib
    unliftio
  ];

  inherit ghcWithPackages;
}
