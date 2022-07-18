{
  add-sandbox,
  digestif,
  emacs,
  rubber,
  stdenv,
  texlive,
}:
let
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
  };
in add-sandbox [emacs digestif] drv
