let
  version = "1bd4bbd49bef217a3d1adea43498270d6e779d65";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "sha256:1fx6nqz8x9biwlwsnh67z5qz0fmrdgr01yvmdw2cw9xjx8hyss3s";
  };
in import pkgs
