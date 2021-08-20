{
  adventlib,
  haskell-pkg,
  haskellPackages,
  stdenv,
}:
let
  haskell-libs = with haskellPackages; [
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

  inherit haskellPackages haskell-libs;
}
