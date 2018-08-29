# Get the binary from convox and fix the interpreter path
#
# I expect the convox, but I haven't found a versioned link. Convox
# documentation asserts that you should install whatever is in that url, on
# every build ...
{
  fetchurl,
  stdenv,
}:

stdenv.mkDerivation rec {
  name = "convox";
  src = fetchurl {
    url = "https://convox.com/cli/linux/convox";
    sha256 = "1191jr796hykrvyg8wggl2ddxqlqnphjafh1a0nxsvqbaybiipl9";
  };

  phases = [ "installPhase" "fixupPhase" ];

  installPhase = ''
    mkdir -p "$out/bin"
    install -v -m755 "$src" "$out/bin/convox"
    patchelf --set-interpreter $(cat ${stdenv.cc}/nix-support/dynamic-linker) \
        $out/bin/convox
  '';
}
  
