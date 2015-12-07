{
  fetchurl,
  simple-java-builder,
  stdenv,
}:
let
  version = "1.4.3";
in
stdenv.mkDerivation {
  name = "java-mailapi-${version}";

  src = fetchurl {
    url =  "http://search.maven.org/remotecontent?filepath=javax/mail/mailapi/${version}/mailapi-${version}.jar";
    sha256 = "09rks869a1smaic0g79ljpc404mq7k7i7hdpmbl59icc4kny8fz8";
  };
  builder = simple-java-builder;
}
