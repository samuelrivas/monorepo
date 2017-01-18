{
  fetchurl,
  scala,
  simple-java-builder,
  stdenv,
}:
let
  version = "1.13.4";
  scala_minor = with stdenv.lib;
    concatStringsSep "." (take 2 (splitString "." (getVersion scala)));
in
stdenv.mkDerivation {
  name = "scalacheck-${version}";

  # TODO: We would need a map matching compiler versions to sources here. This
  # one works only for scala 2.12
  src = fetchurl {
    url =  "https://www.scalacheck.org/files/scalacheck_${scala_minor}-${version}.jar";
    sha256 = "11q1vzx5xz97yxdpqqpdwc9pzbyavw1zi7d11xwrs3d11xjfc9j5";
  };
  builder = simple-java-builder;
}
