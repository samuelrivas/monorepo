{
  generic-lens,
  haskell-pkg,
  hedgehog,
  kattis-cli,
  lens,
  parselib,
  perlude,
}: let
  drv = haskell-pkg {
    name = "kattis-haskell";
    src = ./../src;
    haskell-libs = [
      generic-lens
      hedgehog
      lens
      parselib
      perlude
    ];
    extra-shell-native-build-inputs = [kattis-cli];
  };
in
  drv
