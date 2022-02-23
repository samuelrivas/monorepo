{
  haskell-lib-pkg,
  haskellPackages,
}:
let
  haskell-libs = with haskellPackages; [
  ];
in haskell-lib-pkg {
  name = "boollib";
  src = ./../src;

  inherit haskellPackages haskell-libs;
}
