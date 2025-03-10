{
  asyncq,
  cpplint,
  empty-builder,
  gcc,
  gdb,
  graphlib,
  less,
  lib,
  rapidcheck,
  rndlib,
  stdenv,
  strace,
  valgrind,
}:
stdenv.mkDerivation {
  src = ./../src;
  name = "algos-n-fun";
  nativeBuildInputs =
    [
      asyncq
      cpplint
      gcc
      graphlib
      less
      rapidcheck
      rndlib
    ]
    ++ lib.optionals (!stdenv.isDarwin)
    [
      gdb
      strace
      valgrind
    ];
  builder = empty-builder;
}
