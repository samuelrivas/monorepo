{
  emacs,
  haskellPackages,
  sandbox,
  stdenv,
}:
let
  wanted-packages = with haskellPackages; [
    mtl
    text
  ];
  haskell-packages-selector = pkgs: wanted-packages;
  ghc = haskellPackages.ghcWithPackages haskell-packages-selector;
in
stdenv.mkDerivation rec {

  name = "ds-processing";
  src = ./../src;

  buildInputs = [
    ghc
    haskellPackages.hlint
  ] ++ (if sandbox then [(emacs.override { inherit ghc; })] else []);

  installPhase = ''
    mkdir -p $out/bin
    cp ../build/bin/* $out/bin # */ emacs ...
  '';
}
