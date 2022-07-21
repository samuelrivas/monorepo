{
  haskell-lib-pkg,
  haskellPackages,
}:
let
  haskell-libs = with haskellPackages; [
    lens
    parselib
    unliftio
  ];
in haskell-lib-pkg {
  name = "adventlib";
  src = ./../src;

  inherit haskellPackages haskell-libs;
}
