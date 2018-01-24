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
  sandbox-extras = if sandbox then {
    builder = empty-builder;
    DEBUG = "true";
  } else { };
in
stdenv.mkDerivation (sandbox-extras // {
  src = ./../src;
  name = "randq";
  buildInputs = [
    gcc
  ] ++ (if sandbox then extra-sandbox else []);

  installPhase = ''
    mkdir -p "$out/bin"
    cp ../build/install/bin/randq $out/bin
  '';
})
