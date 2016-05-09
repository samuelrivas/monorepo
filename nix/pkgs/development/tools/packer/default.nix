{
  fetchzip,
  stdenv,
}:

let
  version = "0.10.1";
  package-system-names = {
    x86_64-darwin = "darwin_amd64";
    x86_64-linux = "linux_amd64";
    x86_64-freebsd = "freebsd_amd64";
    x86_64-openbsd = "openbsd_amd64";
  };
  package-system-name = builtins.getAttr stdenv.system package-system-names;
in

# doesn't work without stringification for some reason
assert package-system-names ? "${stdenv.system}";

stdenv.mkDerivation rec {
  name = "packer-${version}";

  src = fetchzip {
    stripRoot = false;
    url = "https://releases.hashicorp.com/packer/${version}/packer_${version}_${package-system-name}.zip";
    sha256 = "06yb3pjvk39b3mwkk3wy7lzl9rxngq7b7wjvscbz3f47s6v4vh8m";
  };

  platforms = builtins.attrNames package-system-names;

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
