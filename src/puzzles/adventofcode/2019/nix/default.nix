{
  emacs-for-haskell,
  haskellPackages,
  sandbox ? false,
  stdenv,
} :
let
  wanted-packages = with haskellPackages; [
    ansi-terminal
    fingertree
    generic-lens
    hashmap
    lens
    monad-loops
    readline
    regex-pcre
    writer-cps-mtl
  ];
  haskell-packages-selector = _: wanted-packages;
  ghc = haskellPackages.ghcWithPackages haskell-packages-selector;
in
stdenv.mkDerivation rec {

  name = "adventofcode-2019";

  src = ./../src;

  buildInputs = [
    ghc
  ]
  ++ (if sandbox
      then [(emacs-for-haskell ghc) haskellPackages.hoogle]
      else []);

  installPhase = ''
    mkdir -p $out/bin
    cp ../build/bin/* $out/bin
  '';
}
