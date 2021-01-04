{
  adventlib,
  haskell-pkg,
  haskellPackages,
  stdenv,
}:
let
  haskell-packages-selector = p: with p; [
    adventlib
    ansi-terminal
    deque
    fingertree
    generic-lens
    lens
    matrix
    monad-loops
    multiset
    readline
    regex-tdfa
    regex-tdfa-text
    unliftio
    unordered-containers
    writer-cps-mtl
  ];
in
haskell-pkg {

  name = "adventofcode-2020";
  src = ./../src;

  extra-drv = {
    ADVENT_INPUT_DIR = ./../src/inputs;
    shellHook = ''
      export ADVENT_INPUT_DIR=inputs
    '';
  };

  inherit haskell-packages-selector;
}
