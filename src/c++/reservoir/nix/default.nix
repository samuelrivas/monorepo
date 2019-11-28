{
  bc,
  cpplint,
  empty-builder,
  gdb,
  gcc,
  rndlib,
  sandbox,
  sh-lib,
  stdenv,
  strace,
  valgrind,
}:
let
  extra-sandbox = [
    gdb
    strace
  ];
  sandbox-extras = if sandbox then {
    builder = empty-builder;
  } else { };
in
stdenv.mkDerivation (sandbox-extras // {
  src = ./../src;
  name = "reservoir";
  buildInputs = [
    bc
    cpplint
    gcc
    rndlib
    sh-lib
    valgrind
  ] ++ (if sandbox then extra-sandbox else []);

  doCheck = true;
  prePatch = ''
    patchShebangs .
  '';
  installPhase = ''
    mkdir -p "$out/bin"
    cp ../build/install/bin/reservoir $out/bin
  '';
})
