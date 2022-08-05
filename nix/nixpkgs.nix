let
  version = "478f3cbc8448b5852539d785fbfe9a53304133be";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "sha256:1xdr33l7hg7v81dm4m5a8hysa33z6gqisq0hyrdkds3yrnnxfp2w";
  };
in import pkgs
