{
  emacs-for-haskell,
  empty-builder,
  haskellPackages,
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

  name = "boardgamer";
  src = ./../src;

  buildInputs = [
    ghc
    haskellPackages.hlint
  ] ++ (if sandbox then [(emacs-for-haskell ghc)] else []);

  installPhase = ''
    mkdir -p $out/bin
    cp ../build/bin/* $out/bin # */ emacs ...
  '';
}
