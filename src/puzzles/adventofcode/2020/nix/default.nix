{
  adventlib-old-1,
  ansi-terminal,
  boollib,
  deque,
  fingertree,
  generic-lens,
  haskell-pkg,
  lens,
  makeWrapper,
  matrix,
  monad-loops,
  mtl,
  multiset,
  parselib,
  perlude,
  readline,
  regex-tdfa,
  transformers,
  unliftio,
  unordered-containers,
}: let
  advent-input-dir = ./../src/inputs;
in
  haskell-pkg {
    name = "adventofcode-2020";
    src = ./../src;
    haskell-libs = [
      adventlib-old-1
      ansi-terminal
      boollib
      deque
      fingertree
      generic-lens
      lens
      matrix
      monad-loops
      multiset
      mtl
      parselib
      perlude
      readline
      regex-tdfa
      transformers
      unliftio
      unordered-containers
    ];

    extra-build-inputs = [makeWrapper];

    extra-drv = {
      shellHook = ''
        export ADVENT_INPUT_DIR=inputs
      '';
      postFixup = ''
        wrapProgram "$out/bin/advent-2020" \
        --set ADVENT_INPUT_DIR "${advent-input-dir}"
      '';
    };
  }
