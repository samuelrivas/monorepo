{
  haskell-pkg,
  haskellPackages,
  sandbox,
}: haskell-pkg {
  name = "onirim-helper";
  src = ./../src;
  wanted-packages = with haskellPackages; [
    generic-lens
    lens
    multiset
    random-fu
    readline
  ];
  inherit haskellPackages sandbox;
}
