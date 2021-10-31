{
  haskell-pkg,
  haskellPackages,
}:
let
  haskell-libs = with haskellPackages; [
    fingertree
    generic-lens
    lens
    perlude
    unordered-containers
    writer-cps-mtl
  ];
in haskell-pkg {
  name = "searchlib";
  src = ./../src;

  inherit haskellPackages haskell-libs;

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
