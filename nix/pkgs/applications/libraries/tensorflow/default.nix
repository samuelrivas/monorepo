{
  buildPythonPackage,
  fetchurl,
  gcc,
  protobuf3_1,
  pythonPackages,
  stdenv,
  swig,
  zlib,
}:

let
  python-protobuf3_1 = (pythonPackages.protobufBuild protobuf3_1).override { doCheck = false; };
in
buildPythonPackage rec {
  name = "tensorflow";
  version = "0.12.1";
  format = "wheel";

  src = fetchurl {
    url = if stdenv.isDarwin then
      "https://storage.googleapis.com/tensorflow/mac/cpu/tensorflow-${version}-py2-none-any.whl" else
      "https://storage.googleapis.com/tensorflow/linux/cpu/tensorflow-${version}-cp27-none-linux_x86_64.whl";
    sha256 = if stdenv.isDarwin then
      "1gjybh3j3rn34bzhsxsfdbqgsr4jh50qyx2wqywvcb24fkvy40j9" else
      "0m45ypkc63fb9yj4lx193vah9f07v0ajq7nqd1hlwnmzg3mw1m3m";
  };

  propagatedBuildInputs = with pythonPackages; [ numpy six python-protobuf3_1 swig mock];

  preFixup = ''
    RPATH="${stdenv.lib.makeLibraryPath [ gcc.cc.lib zlib ]}"
    find $out -name '*.so' -exec patchelf --set-rpath "$RPATH" {} \;
  '';

  doCheck = false;

  meta = {
    description = "TensorFlow helps the tensors flow (no gpu support)";
    homepage = http://tensorflow.org;
    license = stdenv.lib.licenses.asl20;
    platforms = with stdenv.lib.platforms; linux ++ darwin;
  };
}