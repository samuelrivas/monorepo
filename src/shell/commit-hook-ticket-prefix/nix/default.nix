{
  git,
  gnused,
  makeWrapper,
  sh-lib,
  stdenv,
}:
let
  lib-dir = sh-lib + "/lib";
in
stdenv.mkDerivation rec {
  name = "commit-hook-ticket-prefix";

  src = ./../src;

  buildInputs = [ makeWrapper ];

  dontBuild = true;
  dontStrip = true;
  dontPatchELF = true;

  installPhase = ''
    mkdir -p "$out/bin"
    mkdir -p "$out/lib"

    cp -r "$src/bin/"* "$out/bin"
    cp -r "$src/lib/"* "$out/lib"
  '';

  inherit git gnused;

  postFixup = ''
    wrapProgram "$out/bin/commit-hook-ticket-prefix-install" \
      --suffix-each PATH : "$git/bin"                        \
      --set SH_LIB "${lib-dir}"

    wrapProgram "$out/lib/commit-hook-ticket-prefix" \
      --suffix-each PATH : "$git/bin $gnused/bin"
  '';
}
