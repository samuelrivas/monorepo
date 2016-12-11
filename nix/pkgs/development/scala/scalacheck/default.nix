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
  # one works only for scala 2.11
  src = fetchurl {
    url =  "https://www.scalacheck.org/files/scalacheck_${scala_minor}-${version}.jar";
    sha256 = "1mwgbzp1plb3znsbh450nzg0xlnkksb2f24dhll7vds3sr5gylp3";
  };
  builder = simple-java-builder;
}
