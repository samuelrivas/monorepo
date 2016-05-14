/* This derivation is currently only meant to be used for sandboxing (see make
 * nix-shell)
 * It configures ghc with the desired packages (wanted-packages), and adds an
 * emacs with access to that ghc
 */
{
  emacs,
  empty-builder,
  haskellPackages,
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
    (emacs.override { inherit ghc; })
    ghc
  ];

  builder = empty-builder;
}
