{
  add-sandbox,
  bc,
  cpplint,
  gdb,
  gcc,
  lib,
  makeWrapper,
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
  doCheck = stdenv.isLinux; # valgrind is boken on darwin;
  drv = stdenv.mkDerivation ({
      inherit doCheck;
      src = ./../src;
      name = "reservoir";
      buildInputs = [
        rndlib
        sh-lib
      ];
      nativeBuildInputs =
        [
          bc
          cpplint
          gcc
          rndlib
          sh-lib
        ]
        ++ lib.optional doCheck [valgrind]
        ++ lib.optional stdenv.isDarwin [makeWrapper];

      prePatch = ''
        patchShebangs .
      '';
      installPhase = ''
        mkdir -p "$out/bin"
        cp ../build/install/bin/reservoir $out/bin
      '';
    }
    // lib.optionalAttrs stdenv.isDarwin {
      # Loading c++libraries is broken in Darwin
      postFixup = ''
        wrapProgram "$out/bin/reservoir" \
          --set DYLD_LIBRARY_PATH "${rndlib}/lib"
      '';
    });
in
  add-sandbox extra-sandbox drv
