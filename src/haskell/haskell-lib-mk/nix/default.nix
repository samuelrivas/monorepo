{
  mk-conf-file,
  stdenv,
}:
let
  arch = builtins.replaceStrings ["darwin"] ["osx"] stdenv.targetPlatform.system;
in
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

    addArchInfo() {
      export ARCH="${arch}"
      export SHARED_LIB_EXTENSION="${stdenv.targetPlatform.extensions.sharedLibrary}"
    }

    addEnvHooks "$hostOffset" addHaskellLibMkPath
    addEnvHooks "$hostOffset" addArchInfo
    EOF
  '';
}
