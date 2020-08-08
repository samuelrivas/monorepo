let
  version = "e23e05452c67ce406debffa831290fb3abaabf0e";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "10wlcm20bvak8cxjhfvmn0vm4n9da3zl19026h66zc1wfmcqgrkp";
  };
in import pkgs
