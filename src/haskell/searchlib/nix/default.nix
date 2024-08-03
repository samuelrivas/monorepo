{
  fingertree,
  generic-lens,
  haskell-lib-pkg,
  lens,
  mtl,
  perlude,
  transformers,
  unordered-containers,
}:
haskell-lib-pkg {
  name = "searchlib";
  src = ./../src;
  haskell-libs = [
    fingertree
    generic-lens
    lens
    mtl
    perlude
    transformers
    unordered-containers
  ];
}
