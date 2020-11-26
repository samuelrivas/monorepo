{
  haskell-pkg,
  haskellPackages,
}: haskell-pkg {
  name = "adventlib";
  src = ./../src;
  haskell-packages-selector = p: [
    p.lens
    p.unliftio
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
