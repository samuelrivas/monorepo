let
  version = "bb9da1323ebe39ff98a8412b963078a3a2bd2687";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "0979wgymadgmnyi0qh8r34gn5sfqi707mmpxzp2sj3jxb2alywxy";
  };
in import pkgs
