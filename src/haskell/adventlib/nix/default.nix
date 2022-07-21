{
  haskell-lib-pkg,
  haskellPackages,
}:
let
  haskell-libs = with haskellPackages; [
    generic-lens
    lens
    parselib
    unliftio
  ];
in haskell-lib-pkg {
  name = "adventlib";
  src = ./../src;

  inherit haskellPackages haskell-libs;
}
