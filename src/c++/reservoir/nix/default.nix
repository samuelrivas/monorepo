{
  add-sandbox,
  bc,
  cpplint,
  gdb,
  gcc,
  rndlib,
  sh-lib,
  stdenv,
  strace,
  valgrind,
}: let
  extra-sandbox = [
    gdb
    strace
  ];
  drv = stdenv.mkDerivation {
    src = ./../src;
    name = "reservoir";
    buildInputs = [
      bc
      cpplint
      gcc
      rndlib
      sh-lib
      valgrind
    ];

    doCheck = true;
    prePatch = ''
      patchShebangs .
    '';
    installPhase = ''
      mkdir -p "$out/bin"
      cp ../build/install/bin/reservoir $out/bin
    '';
  };
in
  add-sandbox extra-sandbox drv
