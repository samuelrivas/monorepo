{
  boost,
  bc,
  cpplint,
  empty-builder,
  gdb,
  gcc,
  sandbox,
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
  sandbox-extras = if sandbox then {
    builder = empty-builder;
  } else { };
in
stdenv.mkDerivation (sandbox-extras // {
  src = ./../src;
  name = "reservoir";
  buildInputs = [
    boost
    cpplint
    gcc
    sqlite
  ] ++ (if sandbox then extra-sandbox else []);

  builder = empty-builder;
})
