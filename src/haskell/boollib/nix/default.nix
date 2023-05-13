{
  ghcWithPackages,
  haskell-lib-pkg,
}:
haskell-lib-pkg {
  name = "boollib";
  src = ./../src;
  haskell-libs = [ ];

  inherit ghcWithPackages;
}
