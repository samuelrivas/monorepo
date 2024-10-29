{
  fingertree,
  generic-lens,
  haskell-lib-pkg,
  lens,
  monad-emit,
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
    monad-emit
    mtl
    perlude
    transformers
    unordered-containers
  ];
}
