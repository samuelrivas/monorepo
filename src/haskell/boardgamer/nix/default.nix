{
  haskell-pkg,
  haskellPackages,
}: haskell-pkg {
  name = "boardgamer";
  src = ./../src;
  wanted-packages = with haskellPackages; [
    random-fu
  ];
  inherit haskellPackages;
}
