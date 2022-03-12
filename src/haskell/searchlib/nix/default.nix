{
  haskell-lib-pkg,
  haskellPackages,
}:
let
  haskell-libs = with haskellPackages; [
    fingertree
    generic-lens
    lens
    perlude
    unordered-containers
    writer-cps-mtl
  ];
in haskell-lib-pkg {
  name = "searchlib";
  src = ./../src;

  inherit haskellPackages haskell-libs;
}
