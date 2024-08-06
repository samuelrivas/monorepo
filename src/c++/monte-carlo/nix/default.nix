{
  boost,
  cpplint,
  lib,
  gdb,
  gcc,
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
    name = "monte-carlo";
    buildInputs = [
      boost
    ];
    nativeBuildInputs =
      [
        cpplint
        gcc
      ]
      ++ lib.optional (!stdenv.isDarwin) valgrind;

    # FIXME: Tests require valgrind, which is not available for Darwin in
    # nixpkgs. We could still refactor them to run without valgrind on mac
    doCheck = !stdenv.isDarwin;
    installPhase = ''
      mkdir -p "$out/bin"
    '';
  };
in
  drv
  // {
    passthru.dev-shell =
      drv
      // {
        nativeBuildInputs =
          drv.nativeBuildInputs
          ++ lib.optionals (!stdenv.isDarwin) extra-sandbox;
      };
  }
