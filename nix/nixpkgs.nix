let
  version = "7adc9c14ec74b27358a8df9b973087e351425a79";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "sha256:09dpk1c06jjcvxccgsgaz6a843vjgaglqpyix07fdc34hcrs5zmh";
  };
in import pkgs
