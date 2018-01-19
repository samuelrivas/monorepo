{
  cpplint,
  empty-builder,
  gdb,
  gcc,
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
    strace
    valgrind
  ];
  builder = empty-builder;
}
