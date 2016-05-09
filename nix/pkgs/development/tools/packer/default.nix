{
  stdenv,
  fetchzip,
}:

let
  version = "0.10.1";
in
stdenv.mkDerivation rec {
  name = "packer-${version}";
  src = fetchzip {
    stripRoot = false;
    url = "https://releases.hashicorp.com/packer/${version}/packer_${version}_linux_amd64.zip";
    sha256 = "06yb3pjvk39b3mwkk3wy7lzl9rxngq7b7wjvscbz3f47s6v4vh8m";
  };

  phases = [ "checkPhase" "installPhase" ];

  doCheck = true;

  # Just make sure it runs
  checkPhase = ''
    $src/packer version > /dev/null
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp $src/packer $out/bin
  '';
}
