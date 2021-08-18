{
  haskell-pkg,
  ghcWithPackages,
  fingertree,
  generic-lens,
  lens,
  unliftio,
  writer-cps-mtl,
}:
let
  haskell-libs = [
    fingertree
    generic-lens
    lens
    unliftio
    writer-cps-mtl
  ];
in haskell-pkg {
  name = "adventlib";
  src = ./../src;

  inherit ghcWithPackages haskell-libs;

  extra-drv = rec {
    makeFlags = "PREFIX=$out";
    propagatedBuildInputs = haskell-libs;
    installPhase = ''
      make ${makeFlags} install
    '';
    # Silently required by ghcWithPackages, for some reason
    isHaskellLibrary = true;
  };
}
