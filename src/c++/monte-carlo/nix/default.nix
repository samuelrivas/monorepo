{
  add-sandbox,
  boost,
  cpplint,
  empty-builder,
  gdb,
  gcc,
  stdenv,
  strace,
  valgrind,
}:
let
  extra-sandbox = [
    gdb
    strace
  ];
  drv = stdenv.mkDerivation {
    src = ./../src;
    name = "monte-carlo";
    buildInputs = [
      boost
      cpplint
      gcc
      valgrind
    ];

    doCheck = true;
    installPhase = ''
      mkdir -p "$out/bin"
    '';
  };
in add-sandbox extra-sandbox drv
