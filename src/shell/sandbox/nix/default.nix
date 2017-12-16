{
  gawk,
  makeWrapper,
  nix,
  nix-root,
  sh-lib,
  stdenv,
}:
let
  lib-dir = sh-lib + "/lib";
in
stdenv.mkDerivation rec {
  name = "sandbox";

  # TODO fix this so that we don't include ourselves
  src = ./../src;

  buildInputs = [ makeWrapper ];

  dontBuild = true;
  dontStrip = true;
  dontPatchELF = true;

  installPhase = ''
    mkdir -p "$out/bin"
    cp -r bin/sandbox "$out/bin"
  '';

  inherit gawk
          nix;

  postFixup = ''
    SCHIBSTED_SH_LIB="$out/lib"

    wrapProgram "$out/bin/sandbox"              \
      --suffix-each PATH : "$nix/bin $gawk/bin" \
      --set SH_LIB "${lib-dir}"                 \
      --set CUSTOM_PKGS "${nix-root}"
  '';
}
