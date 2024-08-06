{
  add-sandbox,
  asyncq,
  cpplint,
  empty-builder,
  gcc,
  gdb,
  graphlib,
  kcachegrind, # Needs the enviornment in PATH to work ATM
  less,
  lib,
  rapidcheck,
  rndlib,
  stdenv,
  strace,
  valgrind,
}:
add-sandbox [] (stdenv.mkDerivation {
  src = ./../src;
  name = "algos-n-fun";
  nativeBuildInputs = [
    asyncq
    cpplint
    gcc
    graphlib
    less
    rapidcheck
    rndlib
  ] ++ lib.optionals (!stdenv.isDarwin)
    [ gdb
      kcachegrind
      strace
      valgrind
    ];
  builder = empty-builder;
})
