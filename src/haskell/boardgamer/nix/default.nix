{
  haskell-pkg,
  haskellPackages,
}: haskell-pkg {
  name = "boardgamer";
  src = ./../src;
  haskell-libs = with haskellPackages; [
    random-fu
  ];
  inherit haskellPackages;
}
