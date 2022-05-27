{ stdenv }:
stdenv.mkDerivation {
  name = "test-haskell-mk";
  src = ./../src;
  installPhase = ''
    mkdir -p "$out/lib"
    mkdir -p "$out/nix-support"
    cp lib/test-haskell.mk $out/lib

    cat > $out/nix-support/setup-hook <<EOF
    addTestHaskellMkPath() {
      export TEST_HASKELL_MK="$out/lib/test-haskell.mk"
    }

    addEnvHooks "$hostOffset" addTestHaskellMkPath
    EOF
  '';
}
