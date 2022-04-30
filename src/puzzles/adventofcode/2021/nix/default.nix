{
  haskell-pkg,
  haskellPackages,
  makeWrapper,
  stdenv,
}:
let
  haskell-libs = with haskellPackages; [
    adventlib
    ansi-terminal
    boollib
    deque
    fingertree
    generic-lens
    lens
    matrix
    monad-emit
    monad-loops
    multiset
    parselib
    perlude
    readline
    regex-tdfa
    searchlib
    unliftio
    unordered-containers
    writer-cps-mtl
    zippers
  ];
  advent-input-dir = ./../src/inputs;
in
haskell-pkg {

  name = "adventofcode-2021";
  src = ./../src;

  extra-build-inputs = [ makeWrapper ];

  extra-drv = {
    shellHook = ''
      export ADVENT_INPUT_DIR=inputs
    '';
    postFixup = ''
      wrapProgram "$out/bin/advent-2021" \
      --set ADVENT_INPUT_DIR "${advent-input-dir}"
    '';

  };

  inherit haskellPackages haskell-libs;
}
