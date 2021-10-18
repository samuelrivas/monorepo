{
  haskell-pkg,
  haskellPackages,
}:
let
  haskell-libs = with haskellPackages; [
  ];
in haskell-pkg {
  name = "perlude";
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
