{
  haskell-pkg,
  haskellPackages,
}: haskell-pkg {
  name = "name-generator";
  src = ./../src;
  wanted-packages = with haskellPackages; [
    random-fu
  ];
  inherit haskellPackages;
}
