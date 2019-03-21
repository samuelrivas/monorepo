{
  boost,
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
  ];
  sandbox-extras = if sandbox then {
    builder = empty-builder;
  } else { };
in
stdenv.mkDerivation (sandbox-extras // {
  src = ./../src;
  name = "monte-carlo";
  buildInputs = [
    boost
    cpplint
    gcc
    valgrind
  ] ++ (if sandbox then extra-sandbox else []);

  doCheck = true;
  installPhase = ''
    mkdir -p "$out/bin"
  '';
})
