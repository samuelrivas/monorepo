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
    lens
    monad-loops
    readline
    regex-pcre
    unliftio
    unordered-containers
    writer-cps-mtl
  ];
  haskell-packages-selector = _: wanted-packages;
  ghc = haskellPackages.ghcWithPackages haskell-packages-selector;
in
stdenv.mkDerivation rec {

  name = "adventofcode-2019";
  src = ./../src;

  ADVENT_INPUT_DIR = ./../src/inputs;

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
