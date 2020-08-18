{
  haskell-pkg,
  haskellPackages,
}: haskell-pkg {
  name = "advent-lib";
  src = ./../src;
  haskell-packages-selector = p: [
    p.random-fu
  ];
  inherit haskellPackages;
  extra-drv = {
    installPhase = ''
      make INSTALL-PATH=$out install
    '';
    isHaskellLibrary = true;
  };
}
