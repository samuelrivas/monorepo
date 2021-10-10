{
  gawk,
  git,
  gnugrep,
  gnused,
  lib,
  makeWrapper,
  pdftk,
  python,
  sh-lib,
  stdenv,
  which,
  xbacklight,
  xclip,
  xrandr,
  xset,
}:

stdenv.mkDerivation rec {
  name = "assorted-scripts-0.0.0";

  src = ./../src;

  inherit
    gawk
    git
    gnugrep
    gnused
    pdftk
    python
    which
    xbacklight
    xclip
    xrandr
    xset;

  buildInputs = [
    makeWrapper
    sh-lib
  ];

  dontBuild = true;
  dontStrip = true;
  dontPatchELF = true;

  installPhase = ./install.sh;
  postFixup = ./post-fixup.sh;

  meta = {
    description = "A collection of barely useful scripts";
    homepage = "https://github.com/samuelrivas/monorepo/src/shell/assorted-scripts";
    license = lib.licenses.bsd3;
    maintainers = [ "Samuel Rivas <samuelrivas@gmail.com>" ];
  };
}
