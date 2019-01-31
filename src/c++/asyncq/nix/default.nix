{
  stdenv,
}:
stdenv.mkDerivation {
  src = ./../src;
  name = "asyncq";
  buildInputs = [
  ];
  installPhase = ''
    mkdir -p "$out/include"
    cp $src/include/* $out/include
  '';
}
