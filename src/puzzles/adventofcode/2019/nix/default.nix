{
  haskell-pkg,
  ghcWithPackages,
  adventlib,
  ansi-terminal,
  fingertree,
  generic-lens,
  lens,
  monad-loops,
  readline,
  regex-pcre,
  unliftio,
  unordered-containers,
  writer-cps-mtl,
  stdenv,
}:
let
  haskell-libs = [
    adventlib
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
in
haskell-pkg {

  name = "adventofcode-2019";
  src = ./../src;

  extra-drv = {
    ADVENT_INPUT_DIR = ./../src/inputs;
  };

  inherit ghcWithPackages haskell-libs;
}
