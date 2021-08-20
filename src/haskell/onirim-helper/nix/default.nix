{
  haskell-pkg,
  haskellPackages,
}: haskell-pkg {
  name = "onirim-helper";
  src = ./../src;
  haskell-libs = with haskellPackages; [
    generic-lens
    lens
    multiset
    random-fu
    readline
  ];
  inherit haskellPackages;
}
