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
  name = "monte-carlo";
  buildInputs = [
    cpplint
    gcc
  ] ++ (if sandbox then extra-sandbox else []);

  doCheck = true;
  installPhase = ''
    mkdir -p "$out/bin"
  '';
})
