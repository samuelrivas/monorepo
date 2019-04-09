{
  emacs,
  empty-builder,
  haskellPackages,
  sandbox,
  stdenv,
}:
let
  wanted-packages = with haskellPackages; [
    multiset
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
  ] ++ (if sandbox then [(emacs.override { inherit ghc; })] else []);

  installPhase = ''
    mkdir -p $out/bin
    cp ../build/bin/* $out/bin
  '';
}
