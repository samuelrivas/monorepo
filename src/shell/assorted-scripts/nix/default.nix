{
  gawk,
  lib,
  makeWrapper,
  pdftk,
  python,
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
    pdftk
    python
    which
    xbacklight
    xclip
    xrandr
    xset;

  buildInputs = [
    makeWrapper
  ];

  dontBuild = true;
  dontStrip = true;
  dontPatchELF = true;

  installPhase = ./install.sh;
  postFixup = ./post-fixup.sh;

  meta = {
    description = "A collection of barely useful scripts";
    homepage = https://github.com/samuelrivas/assorted-scripts;
    license = lib.licenses.bsd3;
    maintainers = [ "Samuel Rivas <samuelrivas@gmail.com>" ];
  };
}
