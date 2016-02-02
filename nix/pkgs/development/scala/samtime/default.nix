{
  scala,
  stdenv,
}:
stdenv.mkDerivation {
  src = ./../../../../../src/scala/samtime/src;
  name = "samtime-0.0.0";
  buildInputs = [
    scala
  ];
  installPhase = ''
    mkdir -p "$out/share/java"
    echo $out
    echo    cp ../build/*.jar "$out/share/java" # */ emacs ...
    cp ../build/*.jar "$out/share/java" # */ emacs ...
  '';
}
