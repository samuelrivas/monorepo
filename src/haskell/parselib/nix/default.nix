{
  haskell-lib-pkg,
  haskellPackages,
}:
let
  haskell-libs = with haskellPackages; [
    parsec
    perlude
  ];
in haskell-lib-pkg {
  name = "parselib";
  src = ./../src;

  inherit haskellPackages haskell-libs;
}
