{
  generic-lens,
  haskell-lib-pkg,
  lens,
  mtl,
  perlude,
  transformers,
  unliftio,
}:
haskell-lib-pkg {
  name = "monad-emit";
  src = ./../src;
  haskell-libs = [
    generic-lens
    lens
    mtl
    perlude
    transformers
    unliftio
  ];
}
