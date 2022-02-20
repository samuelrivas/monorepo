{
  haskell-pkg,
  haskellPackages,
  haskell-lib-mk,
}:
let
  haskell-libs = with haskellPackages; [
    lens
    parselib
    unliftio
  ];
in haskell-pkg {
  name = "adventlib";
  src = ./../src;

  inherit haskellPackages haskell-libs;

  extra-build-inputs = [ haskell-lib-mk ];

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
