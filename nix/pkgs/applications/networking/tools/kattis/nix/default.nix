# This wraps the kattis submission client into a runnable script.
# You need to have a valid ~/.kattisrc file for it to work.
{
  fetchFromGitHub,
  python3Packages,
  writeShellScriptBin,
}: let
  src = fetchFromGitHub {
    owner = "Kattis";
    repo = "kattis-cli";
    rev = "e9b036b291f7aa332c8c52c017b85292f595571c";
    sha256 = "sha256-6m/ICA4X8lLec7K95vv6vv/y5e5Hsxku+qVmOoDFMBw=";
  };
  python = python3Packages.python.withPackages (ps: with ps; [requests lxml]);
in
  writeShellScriptBin "kattis-submit" ''
    exec ${python}/bin/python ${src}/submit.py "$@"
  ''
