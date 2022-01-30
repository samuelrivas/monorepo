{
  haskell-pkg,
  haskellPackages,
}: haskell-pkg {
  name = "mk-conf-file";
  src = ./../src;
  haskell-libs = with haskellPackages; [
    optparse-applicative
    template
  ];
  inherit haskellPackages;
}
