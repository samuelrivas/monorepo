# Adding this package as buildInput adds SH_LIB to the build environment,
# pointing to the lib directory of it
#
# Thus scripts in derivations depending on this one can do, for example,
# `source $SH_LIB/prelude.sh`
{stdenv}:
stdenv.mkDerivation rec {
  name = "sh-lib";

  src = ./../src;

  buildInputs = [];

  dontBuild = true;
  dontStrip = true;
  dontPatchELF = true;

  installPhase = ''
    mkdir -p "$out/lib"
    ls -R
    cp -r lib/prelude.sh "$out/lib"
  '';

  setupHook = ./setup-hook.sh;
}
