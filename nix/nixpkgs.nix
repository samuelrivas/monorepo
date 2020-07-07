let
  commit-sha = "02203c195495aeb5fa1eeffea6cfa8a291de05a8";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${commit-sha}.tar.gz";
    sha256 = "0qawlcy5f19vji19jsfrc13cpc8jdvp7xw617x01sg2dk5g1040q";
  };
in import pkgs
