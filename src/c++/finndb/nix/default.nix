{
  add-dev-shell,
  boost,
  cpplint,
  csvkit,
  empty-builder,
  gcc,
  gdb,
  lib,
  sqlite,
  stdenv,
  strace,
  valgrind,
}: let
  drv = stdenv.mkDerivation {
    src = ./../src;
    name = "finndb";
    buildInputs = [
      boost
    ];
    nativeBuildInputs = [
      cpplint
      csvkit
      gcc
      sqlite
    ];

    builder = empty-builder;
  };
in
  add-dev-shell drv {
    native-build-inputs = lib.optionals (!stdenv.isDarwin) [
      gdb
      strace
      valgrind
    ];
  }
