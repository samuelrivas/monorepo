{
  ghcWithPackages,
  haskell-lib-pkg,
  shellFor,
}:
haskell-lib-pkg {
  name = "perlude";
  src = ./../src;
  haskell-libs = [ ];
  inherit ghcWithPackages shellFor;
}
