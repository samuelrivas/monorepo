{
  haskell-pkg,
  haskellPackages,
  stdenv,
}:
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
haskell-pkg {

  name = "adventofcode-2019";
  src = ./../src;

  ADVENT_INPUT_DIR = ./../src/inputs;

  inherit wanted-packages;
}
