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
  ];
in haskell-lib-pkg {
  name = "prioq";
  src = ./../src;

  inherit haskellPackages haskell-libs;
}
