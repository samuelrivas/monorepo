{
  cpplint,
  gcc,
  stdenv,
}:
stdenv.mkDerivation {
  src = ./../src;
  name = "rndlib";
  nativeBuildInputs = [
    cpplint
    gcc
  ];
  installPhase = ''
    mkdir -p "$out/include"
    cp $src/include/* $out/include

    mkdir -p "$out/lib"
    cp ../build/install/lib/* "$out/lib"
  '';
}
