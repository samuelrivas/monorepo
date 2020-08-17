{
  haskell-pkg,
  haskellPackages,
}: haskell-pkg {
  name = "name-generator";
  src = ./../src;
  haskell-packages-selector = p: [
    p.random-fu
  ];
  inherit haskellPackages;
}
