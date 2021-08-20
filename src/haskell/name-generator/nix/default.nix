{
  haskell-pkg,
  haskellPackages,
}: haskell-pkg {
  name = "name-generator";
  src = ./../src;
  haskell-libs = [
    haskellPackages.random-fu
  ];
  inherit haskellPackages;
}
