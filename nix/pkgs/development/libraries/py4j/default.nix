{
  buildPythonPackage,
  fetchzip,
}:
buildPythonPackage rec {
  version = "0.10.4";
  name = "py4j-${version}";

  src = fetchzip {
    url = "https://pypi.python.org/packages/40/ff/766ee9da27a918240456194d0d48d2e9782304cc2f0396cf65190f3b2c09/py4j-0.10.4.tar.gz";
    sha256 = "0l2mb8xiz2b1dbvf9gkf64alpkrg44bddbh6fmhjj329dv0kqhfx";
  };
}
