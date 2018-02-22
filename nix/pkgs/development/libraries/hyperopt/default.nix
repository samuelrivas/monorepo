{
  buildPythonPackage,
  fetchPypi,
  future,
  networkx,
  nose,
  numpy,
  pymongo,
  scipy,
  six,
}:
buildPythonPackage rec {
  pname = "hyperopt";
  name = "${pname}-${version}";
  version = "0.1";

  src = fetchPypi {
    inherit pname version;
    sha256 = "0bypfr4qi995g0yf5v8ig3p0pazz845ha8n6wjimw5j0fqzr0vjg";
  };

  doCheck = false;

  propagatedBuildInputs = [
    future
    networkx
    nose
    numpy
    pymongo
    scipy
    six
  ];
}
