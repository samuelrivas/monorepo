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
let
  extra-sandbox = [
    cpplint
    gdb
    strace
    valgrind
  ];
  maybe-empty-builder = if sandbox then { builder = empty-builder; } else { };
in
stdenv.mkDerivation (maybe-empty-builder // {
  src = ./../src;
  name = "reservoir";
  buildInputs = [
    gcc
  ] ++ (if sandbox then extra-sandbox else []);

  installPhase = ''
    mkdir -p "$out/bin"
    cp ../build/install/bin/reservoir $out/bin
  '';
})
