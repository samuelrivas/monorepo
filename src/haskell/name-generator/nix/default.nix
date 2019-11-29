{
  emacs-for-haskell,
  haskellPackages,
  haskell-mk,
  sandbox,
  stdenv,
}:
let
  wanted-packages = with haskellPackages; [
    random-fu
  ];
  haskell-packages-selector = pkgs: wanted-packages;
  ghc = haskellPackages.ghcWithPackages haskell-packages-selector;
in
stdenv.mkDerivation rec {

  name = "name-generator";
  src = ./../src;

  buildInputs = [
    ghc
    haskell-mk
    haskellPackages.hlint
  ] ++ (if sandbox then [(emacs-for-haskell ghc)] else []);

  installPhase = ''
    mkdir -p $out/bin
    cp ../build/bin/* $out/bin # */ emacs ...
  '';
}
