{
  add-sandbox,
  digestif,
  emacs,
  inotify-tools,
  rubber,
  stdenv,
  texlive,
}: let
  tex = texlive.combine {
    inherit (texlive) scheme-small;
  };
  drv = stdenv.mkDerivation {
    src = ./../src;
    name = "latex-base";
    buildInputs = [
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
  add-sandbox [emacs digestif inotify-tools] drv
