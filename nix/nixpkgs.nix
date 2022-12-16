# Reference to the latest nixos-22.05
let
  version = "0152de25d49dc16883b65f3e29cfea8d32f68956";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "sha256:1glc71dgs1n8siqpaxzqcyhhaf58s73wqf0s0f0ixhzn4n5scma9";
  };
in import pkgs
