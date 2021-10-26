let
  version = "3b1789322fcbcb5cf51228d732752714f1bf77da";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "sha256:0m6gcl2pzvicwbflsvmccdyf03ki1zl3d9dl8rn8hj1gdgssj6vr";
  };
in import pkgs
