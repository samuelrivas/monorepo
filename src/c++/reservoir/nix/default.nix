{
  bc,
  cpplint,
  empty-builder,
  gdb,
  gcc,
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
    cpplint
    gcc
    valgrind
    bc
  ] ++ (if sandbox then extra-sandbox else []);

  SH_LIB = "${sh-lib}/lib";

  doCheck = true;
  prePatch = ''
    patchShebangs .
  '';
  installPhase = ''
    mkdir -p "$out/bin"
    cp ../build/install/bin/reservoir $out/bin
  '';
})
