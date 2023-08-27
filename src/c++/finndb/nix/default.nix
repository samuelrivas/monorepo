{
  add-sandbox,
  bc,
  boost,
  cpplint,
  csvkit,
  empty-builder,
  gcc,
  gdb,
  sqlite,
  stdenv,
  strace,
  valgrind,
}: let
  extra-sandbox = [
    gdb
    strace
    valgrind
  ];
  drv = stdenv.mkDerivation {
    src = ./../src;
    name = "finndb";
    buildInputs = [
      boost
      cpplint
      csvkit
      gcc
      sqlite
    ];

    builder = empty-builder;
  };
in
  add-sandbox extra-sandbox drv
