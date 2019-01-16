{
  cpplint,
  empty-builder,
  gcc,
  gdb,
  graphlib,
  kcachegrind, # Needs the enviornment in PATH to work ATM
  less,
  rapidcheck,
  sandbox,
  stdenv,
  strace,
  valgrind,
}:
stdenv.mkDerivation {
  src = ./../src;
  name = "algos-n-fun";
  buildInputs = [
    cpplint
    gcc
    gdb
    graphlib
    kcachegrind
    less
    rapidcheck
    strace
    valgrind
  ];
  builder = empty-builder;
}
