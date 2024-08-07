{
  lib,
  fetchFromGitHub,
  gcc,
  stdenv,
}:
stdenv.mkDerivation rec {
  name = "updcat";

  src = fetchFromGitHub {
    owner = "samuelrivas";
    repo = "udp-cat";
    rev = "64164f191751d19dd09dea39173ecff205406176";
    sha256 = "18s55yhxkwh6vhq3gn5v10q5c791x8ca3qs31pw4a0phxc5mr6qz";
  };

  nativeBuildInputs = [gcc];

  installPhase = ''
    mkdir -p "$out/bin"
    cp udp-cat "$out/bin"
  '';

  # It used to work for Solaris, but I haven't tested in years, doesn't
  # definitely compiel on mac with
  meta.platforms = lib.platforms.linux;
}
