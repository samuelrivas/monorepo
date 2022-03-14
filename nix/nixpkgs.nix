let
  version = "0f85665118d850aae5164d385d24783d0b16cf1b";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "sha256:1x60c4s885zlqm1ffvjj09mjq078rqgcd08l85004cijfsqld263";
  };
in import pkgs
