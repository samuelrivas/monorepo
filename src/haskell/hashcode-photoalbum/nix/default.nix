{
  haskell-pkg,
  haskellPackages,
}: haskell-pkg {
  name = "photoalbum";
  src = ./../src;
  haskell-libs = with haskellPackages; [
    multiset
    random-fu
  ];
  inherit haskellPackages;
}
