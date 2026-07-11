{
  add-dev-shell,
  digestif,
  emacs,
  inotify-tools,
  lib,
  rubber,
  stdenv,
  texliveSmall,
}: let
  tex = texliveSmall.withPackages (
    ps: [
      ps.scheme-small
    ]
  );
  drv = stdenv.mkDerivation {
    src = ./../src;
    name = "latex-base";
    nativeBuildInputs = [
      rubber
      tex
    ];
    preBuild = ''
      export HOME="$TMPDIR"
    '';
    installPhase = ''
      mkdir -p "$out"
      cp ../build/*.pdf "$out"
    '';
  };
in
  add-dev-shell drv
  {
    native-build-inputs =
      [emacs digestif]
      ++ lib.optional (!stdenv.isDarwin) inotify-tools;
  }
