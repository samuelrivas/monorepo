{
  argparse,
  beautifulsoup4,
  buildPythonPackage,
  fetchzip,
  requests2,
  flake8,
}:
buildPythonPackage rec {
  version = "0.2";
  name = "oktaauth-${version}";

  src = fetchzip {
    url = "https://pypi.python.org/packages/95/11/8c2d4fce3d6e70841386989f1a227da2028d2cc329b6f816584e2f86c492/oktaauth-0.2.tar.gz";
    sha256 = "0lxx9kqsbp3dn3x4ywwmgc8x133br6g1f7vvj4jb1077z89gjnzw";
  };

  buildInputs = [
    flake8
  ];

  propagatedBuildInputs = [
    argparse
    beautifulsoup4
    requests2
  ];

  # Many tests fail
  doCheck = false;

  meta = {
    description = "foo";
    homepage = "bar";
    license = "baz";
    maintainers = [ ];
  };
}
