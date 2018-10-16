{
  cpplint,
  empty-builder,
  gcc,
  gdb,
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
    kcachegrind
    less
    rapidcheck
    strace
    valgrind
  ];
  builder = empty-builder;
}
