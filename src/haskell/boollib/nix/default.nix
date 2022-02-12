{
  haskell-pkg,
  haskellPackages,
  mk-conf-file,
  haskell-lib-mk
}:
let
  haskell-libs = with haskellPackages; [
  ];
in haskell-pkg {
  name = "boollib";
  src = ./../src;

  inherit haskellPackages haskell-libs;

  extra-build-inputs = [ haskell-lib-mk mk-conf-file ];

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
