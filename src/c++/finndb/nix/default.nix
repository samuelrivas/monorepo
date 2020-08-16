{
  boost,
  bc,
  cpplint,
  csvkit,
  empty-builder,
  gdb,
  gcc,
  sqlite,
  stdenv,
  strace,
  valgrind,
}:
let
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
in drv // {
  sandbox = drv // { buildInputs = drv.buildInputs ++ extra-sandbox; };
}

