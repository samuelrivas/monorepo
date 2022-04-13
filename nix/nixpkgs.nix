let
  version = "e7d63bd0d50df412f5a1d8acfa3caae75522e347";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "sha256:132pc4f9ixisyv4117p2jirmlyl6sd76bfaz33rhlcwakg7bhjm7";
  };
in import pkgs
