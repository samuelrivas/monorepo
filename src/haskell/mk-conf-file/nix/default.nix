{
  optparse-applicative,
  haskell-pkg,
}: haskell-pkg {
  name = "mk-conf-file";
  src = ./../src;
  haskell-libs = [
    optparse-applicative
  ];
}
