/* This derivation is currently only meant to be used for sandboxing (see make
 * nix-shell)
 * It configures ghc with the desired packages (wanted-packages), and adds an
 * emacs with access to that ghc
 */
{
  emacs,
  empty-builder,
  haskellPackages,
  sandbox ? false,
  stdenv,
}:
let
  wanted-packages = with haskellPackages; [ ];
  haskell-packages-selector = pkgs: wanted-packages;
  ghc = haskellPackages.ghcWithPackages haskell-packages-selector;
in
stdenv.mkDerivation rec {

  name = "experiments-haskell-0.0.0";
  src = ./../src;

  buildInputs = [
    ghc
  ] ++ (if sandbox then [(emacs.override { inherit ghc; })] else []);

  installPhase = ''
    mkdir -p $out/bin
    cp ../build/bin/* $out/bin # */ emacs ...
  '';
}
