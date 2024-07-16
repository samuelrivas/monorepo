{
  mk-conf-file,
  stdenv,
}:
stdenv.mkDerivation {
  name = "haskell-lib-mk";
  src = ./../src;
  propagatedBuildInputs = [mk-conf-file];
  installPhase = ''
    mkdir -p "$out/lib"
    mkdir -p "$out/nix-support"
    cp lib/build-haskell-lib.mk $out/lib

    cat > $out/nix-support/setup-hook <<EOF
    addHaskellLibMkPath() {
      export HASKELL_LIB_MK="$out/lib/build-haskell-lib.mk"
    }

    exportArch() {
      export ARCH="${stdenv.system}"
    }

    addEnvHooks "$hostOffset" addHaskellLibMkPath exportArch
    EOF
  '';
}
