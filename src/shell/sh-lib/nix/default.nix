{
  stdenv,
}:

stdenv.mkDerivation rec {
  name = "sh-lib";

  src = ./../src;

  buildInputs = [ ];

  dontBuild = true;
  dontStrip = true;
  dontPatchELF = true;

  installPhase = ''
    mkdir -p "$out/lib"
    ls -R
    cp -r lib/prelude.sh "$out/lib"
  '';
}
