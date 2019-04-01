# Get the binary from convox and fix the interpreter path
{
  fetchurl,
  stdenv,
}:

stdenv.mkDerivation rec {
  name = "convox";
  convox-binary = fetchurl {
    # We used to fetchzip this before as source
    #
    # url = "https://bin.equinox.io/c/jewmwFCp7w9/convox-stable-linux-amd64.tgz";
    #
    # But it is outdated (lots of 404 when we use it) There isn't any other
    # reliable url to download this thing, so we go ahead and get this unhashed,
    # unversioned binary and hope for the best
    url = https://convox.com/cli/linux/convox;
    sha256 = "1r3gp1hnnkw4q3vw4zxxv5h00lvh6lyg4ik391nppk77q1zhzk96";
  };

  phases = [ "installPhase" "fixupPhase" ];

  installPhase = ''
    mkdir -p "$out/bin"
    install -v -m755 "${convox-binary}" "$out/bin/convox"
    patchelf --set-interpreter $(cat ${stdenv.cc}/nix-support/dynamic-linker) \
        $out/bin/convox
  '';
}
