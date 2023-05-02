{
  ghcWithPackages,
  haskell-lib-pkg,
  shellFor,
}:
haskell-lib-pkg {
  name = "boollib";
  src = ./../src;
  haskell-libs = [ ];

  inherit ghcWithPackages shellFor;
}
