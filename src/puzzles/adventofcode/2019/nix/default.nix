{
  adventlib-old-1,
  ansi-terminal,
  fingertree,
  generic-lens,
  ghcWithPackages,
  haskell-pkg,
  lens,
  monad-loops,
  readline,
  regex-pcre,
  searchlib,
  unliftio,
  unordered-containers,
  writer-cps-mtl,
}:
haskell-pkg {

  name = "adventofcode-2019";
  src = ./../src;
  haskell-libs = [
    adventlib-old-1
    ansi-terminal
    fingertree
    generic-lens
    lens
    monad-loops
    readline
    regex-pcre
    searchlib
    unliftio
    unordered-containers
    writer-cps-mtl
  ];

  extra-drv = {
    ADVENT_INPUT_DIR = ./../src/inputs;
  };

  inherit ghcWithPackages;
}
