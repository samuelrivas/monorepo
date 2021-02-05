{
  haskell-pkg,
  haskellPackages,
}: haskell-pkg {
  name = "examplelib";
  src = ./../src;
  haskell-packages-selector = p: [
    p.random-fu
  ];
  inherit haskellPackages;

  extra-drv = rec {
    makeFlags = "PREFIX=$out";
    installPhase = ''
      make ${makeFlags} install
    '';
    # Silently required by ghcWithPackages, for some reason
    isHaskellLibrary = true;
  };
}
