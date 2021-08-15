{
  haskell-pkg,
  haskellPackages,
}:
let
  haskell-packages-selector = p: [
    p.fingertree
    p.generic-lens
    p.lens
    p.unliftio
    p.writer-cps-mtl

  ];
in haskell-pkg {
  name = "adventlib";
  src = ./../src;

  inherit haskellPackages;
  haskell-packages-selector = haskell-packages-selector;

  extra-drv = rec {
    makeFlags = "PREFIX=$out";
    propagatedBuildInputs = haskell-packages-selector haskellPackages;
    installPhase = ''
      make ${makeFlags} install
    '';
    # Silently required by ghcWithPackages, for some reason
    isHaskellLibrary = true;
  };
}
