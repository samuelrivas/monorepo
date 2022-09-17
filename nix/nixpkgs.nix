# Reference to the latest nixos-22.05
let
  version = "d423c0ce15adf573c2045bef5c921b958b674bb4";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "sha256:1mf2188zfjqi5ry1lxk66y3lczyws52b4n496fsp34lpfs8fnsp4";
  };
in import pkgs
