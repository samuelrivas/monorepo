{
  fetchurl,
  scala,
  simple-java-builder,
  stdenv,
}:
let
  version = "1.12.5";
  scala_minor = with stdenv.lib;
    concatStringsSep "." (take 2 (splitString "." (getVersion scala)));
in
stdenv.mkDerivation {
  name = "scalacheck-${version}";

  # TODO: We would need a map matching compiler versions to sources here. This
  # one works only for scala 2.11
  src = fetchurl {
    url =  "https://www.scalacheck.org/files/scalacheck_${scala_minor}-${version}.jar";
    sha256 = "0rbwr432800fbli1zsd3ywvlzd33xyn1ncck90ndgllm71qmhi6k";
  };
  builder = simple-java-builder;
}
