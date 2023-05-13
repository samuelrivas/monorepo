{
  fingertree,
  generic-lens,
  ghcWithPackages,
  haskell-lib-pkg,
  lens,
  perlude,
  unordered-containers,
  writer-cps-mtl,
}:
haskell-lib-pkg {
  name = "searchlib";
  src = ./../src;
  haskell-libs = [
    fingertree
    generic-lens
    lens
    perlude
    unordered-containers
    writer-cps-mtl
  ];

  inherit ghcWithPackages;
}
