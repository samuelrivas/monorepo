{
  asyncq,
  cpplint,
  empty-builder,
  gcc,
  gdb,
  graphlib,
  less,
  lib,
  rapidcheck,
  rndlib,
  stdenv,
  strace,
  valgrind,
}: let
  drv = stdenv.mkDerivation {
    src = ./../src;
    name = "algos-n-fun";
    buildInputs = [
      graphlib
      rapidcheck
      rndlib
      asyncq
    ];
    nativeBuildInputs = [
      cpplint
      gcc
      less
    ];
    installPhase = ''
      mkdir -p "$out/bin"
      cp ../build/install/bin/* $out/bin
    '';
  };
in
  drv
