{
  fetchurl,
  simple-java-builder,
  stdenv,
}:
let
  version = "1.0.0-v20140107";
in
stdenv.mkDerivation {
  name = "java-json-${version}";

  src = fetchurl {
    url =  "http://search.maven.org/remotecontent?filepath=org/everit/osgi/bundles/org.everit.osgi.bundles.org.json/1.0.0-v20140107/org.everit.osgi.bundles.org.json-1.0.0-v20140107.jar";
    sha256 = "14r7q8amrjy28w386d5fw7a40mwd1jjjlmirczlxnhws9gxs9507";
  };
  builder = simple-java-builder;
}
