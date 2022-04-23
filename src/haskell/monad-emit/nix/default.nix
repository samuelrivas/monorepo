{
  haskell-lib-pkg,
  haskellPackages,
}:
let
  haskell-libs = with haskellPackages; [
    generic-lens
    lens
    perlude
    unliftio
    writer-cps-mtl
  ];
in haskell-lib-pkg {
  name = "monad-emit";
  src = ./../src;

  inherit haskellPackages haskell-libs;
}
