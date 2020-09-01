{
  add-sandbox,
  cpplint,
  gdb,
  gcc,
  poco,
  rapidcheck,
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
    name = "simple-cache";
    buildInputs = [
      cpplint
      gcc
      poco
      rapidcheck
      valgrind
    ];

    installPhase = ''
      mkdir -p "$out/bin"
    '';
  };
in add-sandbox extra-sandbox drv
