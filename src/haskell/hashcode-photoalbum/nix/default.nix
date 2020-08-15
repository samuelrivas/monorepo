{
  emacs-for-haskell,
  haskellPackages,
  stdenv,
}:
let
  wanted-packages = with haskellPackages; [
    multiset
    random-fu
  ];
  haskell-packages-selector = pkgs: wanted-packages;
  ghc = haskellPackages.ghcWithPackages haskell-packages-selector;
in
stdenv.mkDerivation rec {

  name = "photoalbum";
  src = ./../src;

  buildInputs = [
    ghc
    haskellPackages.hlint
  ];

  installPhase = ''
    mkdir -p $out/bin
    cp ../build/bin/* $out/bin
  '';
}
