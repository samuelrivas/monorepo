{
  haskell-pkg,
  haskellPackages,
}: haskell-pkg {
  name = "boardgamer";
  src = ./../src;
  haskell-packages-selector = p: with p; [
    random-fu
  ];
  inherit haskellPackages;
}
