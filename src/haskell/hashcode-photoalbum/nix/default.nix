{
  haskell-pkg,
  haskellPackages,
}: haskell-pkg {
  name = "photoalbum";
  src = ./../src;
  wanted-packages = with haskellPackages; [
    multiset
    random-fu
  ];
  inherit haskellPackages;
}
