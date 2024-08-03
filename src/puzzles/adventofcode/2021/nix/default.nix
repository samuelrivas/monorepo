{
  adventlib,
  ansi-terminal,
  boollib,
  deque,
  fingertree,
  generic-lens,
  haskell-pkg,
  lens,
  makeWrapper,
  matrix,
  monad-emit,
  monad-loops,
  mtl,
  multiset,
  parselib,
  perlude,
  readline,
  regex-tdfa,
  searchlib,
  transformers,
  unliftio,
  unordered-containers,
  zippers,
}: let
  advent-input-dir = ./../src/inputs;
in
  haskell-pkg {
    name = "adventofcode-2021";
    src = ./../src;
    haskell-libs = [
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
      mtl
      multiset
      parselib
      perlude
      readline
      regex-tdfa
      searchlib
      transformers
      unliftio
      unordered-containers
      zippers
    ];

    extra-build-inputs = [makeWrapper];

    extra-drv = {
      shellHook = ''
        export ADVENT_INPUT_DIR=inputs
      '';
      postFixup = ''
        wrapProgram "$out/bin/advent-2021" \
        --set ADVENT_INPUT_DIR "${advent-input-dir}"
      '';
    };
  }
