let
  version = "e85f0175e3effe9ba191d66c09e8f1b7d6362d5e";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "sha256:1qr2hmymbzwj8jrz6smcgc04scgwp2v5070x22k4fvh261g1n4zw";
  };
in import pkgs
