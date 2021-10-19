{
  haskell-pkg,
  haskellPackages,
}:
let
  haskell-libs = with haskellPackages; [
    parsec
    perlude
  ];
in haskell-pkg {
  name = "parselib";
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
