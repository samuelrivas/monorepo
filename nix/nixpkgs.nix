# Reference to the latest nixos-22.05
let
  version = "cdead16a444a3e5de7bc9b0af8e198b11bb01804";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "sha256:08gi9r00qa3dg6qw7y2sckja5607b1pnd1z0yxg204pv46nbn768";
  };
in import pkgs
