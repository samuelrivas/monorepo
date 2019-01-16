{
  cpplint,
  stdenv,
}:
stdenv.mkDerivation {
  src = ./../src;
  name = "graphlib";
  buildInputs = [
    cpplint
  ];
  installPhase = ''
    mkdir -p "$out/include"
    cp $src/include/* $out/include

    mkdir -p "$out/lib"
    cp ../build/install/lib/* "$out/lib"
  '';
}
