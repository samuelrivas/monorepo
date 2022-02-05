{
  haskell-pkg,
  haskellPackages,
  mk-conf-file
}:
let
  haskell-libs = with haskellPackages; [
  ];
in haskell-pkg {
  name = "boollib";
  src = ./../src;

  inherit haskellPackages haskell-libs;

  extra-build-inputs = [ mk-conf-file ];

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
