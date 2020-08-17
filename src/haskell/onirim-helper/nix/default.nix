{
  haskell-pkg,
  haskellPackages,
}: haskell-pkg {
  name = "onirim-helper";
  src = ./../src;
  haskell-packages-selector = p: with p; [
    generic-lens
    lens
    multiset
    random-fu
    readline
  ];
  inherit haskellPackages;
}
