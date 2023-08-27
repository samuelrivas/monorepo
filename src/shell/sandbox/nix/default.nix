{
  makeWrapper,
  nix,
  sh-lib,
  stdenv,
}:
stdenv.mkDerivation {
  name = "sandbox";
  src = ./../src;

  nativeBuildInputs = [makeWrapper sh-lib];

  dontBuild = true;
  dontStrip = true;
  dontPatchELF = true;

  installPhase = ''
    mkdir -p "$out/bin"
    cp -r bin/sandbox "$out/bin"
  '';

  postFixup = ''
    wrapProgram "$out/bin/sandbox"    \
      --suffix-each PATH : "${nix}/bin" \
      --set SH_LIB "$SH_LIB"
  '';
}
