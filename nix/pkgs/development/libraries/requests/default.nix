{
  buildPythonPackage,
  chardet,
  urllib3,
  fetchzip,
}:
buildPythonPackage rec {
  version = "2.18.4";
  name = "requests-${version}";

  src = fetchzip {
    url = "https://pypi.python.org/packages/b0/e1/eab4fc3752e3d240468a8c0b284607899d2fbfb236a56b7377a329aa8d09/requests-2.18.4.tar.gz";
    sha256 = "1zfzi2z0vi08bn0grmwlkg9lrnz365f5iy0747zr8slmfhwqbfqg";
  };

  # the tests download stuff from the network
  doCheck=false;

  propagatedBuildInputs = [
    chardet
    urllib3
  ];
}
