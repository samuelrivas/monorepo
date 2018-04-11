# Get the binary from convox and fix the interpreter path
#
# I expect the zip file to change often, but I haven't found a versioned link
# ...
{
  fetchzip,
  stdenv,
}:

stdenv.mkDerivation rec {
  name = "convox";
  src = fetchzip {
    url = "https://bin.equinox.io/c/jewmwFCp7w9/convox-stable-linux-amd64.tgz";
    sha256 = "0hgd52m957agzb8ifn8b281nvdvs7i3fch5a6095hvmxqqykr0ds";
  };

  phases = [ "installPhase" "fixupPhase" ];

  installPhase = ''
    mkdir -p "$out/bin"
    install -v -m755 "$src/convox" "$out/bin/convox"
    patchelf --set-interpreter $(cat ${stdenv.cc}/nix-support/dynamic-linker) \
        $out/bin/convox
  '';
}
  
