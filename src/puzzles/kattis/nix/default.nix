{
  haskell-pkg,
  kattis-cli,
  perlude,
}: let
  drv = haskell-pkg {
    name = "kattis-haskell";
    src = ./../src;
    haskell-libs = [
      perlude
    ];
    extra-shell-native-build-inputs = [kattis-cli];
  };
in
  drv
