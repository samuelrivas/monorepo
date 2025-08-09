{
  haskell-pkg,
  kattis-cli,
  parselib,
  perlude,
}: let
  drv = haskell-pkg {
    name = "kattis-haskell";
    src = ./../src;
    haskell-libs = [
      parselib
      perlude
    ];
    extra-shell-native-build-inputs = [kattis-cli];
  };
in
  drv
