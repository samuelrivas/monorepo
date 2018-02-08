{
  buildPythonPackage,
  fetchzip,
  py4j,
}:
buildPythonPackage rec {
  version = "2.1.2";
  name = "pyspark-${version}";

  propagatedBuildInputs = [
    py4j
  ];

  src = fetchzip {
    url = "https://pypi.python.org/packages/65/d7/2a3b0de1178478fc00201b083d50b3d2d1affe4eac92dad3408219c5c607/pyspark-2.1.2.tar.gz";
    sha256 = "17aj815sr2id6in1sb68yyqjcm45wmrg8jv6wn5vn4v876aqbbk0";
  };
}
