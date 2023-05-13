{
  generic-lens,
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
    generic-lens
    lens
    parselib
    unliftio
  ];

  inherit ghcWithPackages;
}
