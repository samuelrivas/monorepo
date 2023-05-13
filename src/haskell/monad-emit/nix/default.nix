{
  generic-lens,
  ghcWithPackages,
  haskell-lib-pkg,
  lens,
  perlude,
  unliftio,
  writer-cps-mtl,
}:
haskell-lib-pkg {
  name = "monad-emit";
  src = ./../src;
  haskell-libs = [
    generic-lens
    lens
    perlude
    unliftio
    writer-cps-mtl
  ];

  inherit ghcWithPackages;
}
