{
  haskell-pkg,
  haskellPackages,
}: haskell-pkg {
  name = "photoalbum";
  src = ./../src;
  haskell-packages-selector = p: with p; [
    multiset
    random-fu
  ];
  inherit haskellPackages;
}
