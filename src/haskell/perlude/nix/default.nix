{
  haskell-lib-pkg,
  haskellPackages,
}:
let
  haskell-libs = with haskellPackages; [
  ];
in haskell-lib-pkg {
  name = "perlude";
  src = ./../src;

  inherit haskellPackages haskell-libs;
}
