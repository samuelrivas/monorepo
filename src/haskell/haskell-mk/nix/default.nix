{ stdenv }:
stdenv.mkDerivation {
  name = "haskell-mk";
  src = ./../src;
  installPhase = ''
    mkdir -p "$out/lib"
    mkdir -p "$out/nix-support"
    cp lib/build-haskell.mk $out/lib

    cat > $out/nix-support/setup-hook <<EOF
    addHaskellMkPath() {
      export HASKELL_MK="$out/lib/build-haskell.mk"
    }

    addEnvHooks "$hostOffset" addHaskellMkPath
    EOF
  '';
}
