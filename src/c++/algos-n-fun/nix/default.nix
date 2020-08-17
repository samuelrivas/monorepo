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
  rapidcheck,
  rndlib,
  stdenv,
  strace,
  valgrind,
}:
add-sandbox [] (stdenv.mkDerivation {
  src = ./../src;
  name = "algos-n-fun";
  buildInputs = [
    asyncq
    cpplint
    gcc
    gdb
    graphlib
    kcachegrind
    less
    rapidcheck
    rndlib
    strace
    valgrind
  ];
  builder = empty-builder;
})
