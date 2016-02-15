{
  empty-builder,
  ghcWithPackages,
  stdenv,
}:
let
  haskell-packages-selector = pkgs: [pkgs.lens];
in
stdenv.mkDerivation rec {

  name = "experiments-haskell-0.0.0";
  src = ./../src;

  buildInputs = [
    (ghcWithPackages haskell-packages-selector)
  ];

  builder = empty-builder;
}
