{
  haskell-pkg,
  haskellPackages,
}: haskell-pkg {
  name = "clean-clocks";
  src = ./../src;
  haskell-libs = with haskellPackages; [
    lens
    parselib
    perlude
  ];
  inherit haskellPackages;
}