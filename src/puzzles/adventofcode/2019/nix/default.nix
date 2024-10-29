{
  adventlib-old-1,
  ansi-terminal,
  fingertree,
  generic-lens,
  haskell-pkg,
  lens,
  makeWrapper,
  monad-loops,
  readline,
  regex-pcre,
  unliftio,
  unordered-containers,
}: let
  advent-input-dir = ./../src/inputs;
in
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
      unliftio
      unordered-containers
    ];

    extra-build-inputs = [makeWrapper];

    extra-drv = {
      shellHook = ''
        export ADVENT_INPUT_DIR=inputs
      '';
      postFixup = ''
        wrapProgram "$out/bin/advent-2019" \
        --set ADVENT_INPUT_DIR "${advent-input-dir}"
      '';
    };
  }
