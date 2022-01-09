let
  version = "36480448d470bf41bb21267cf9062a1542c4a95f";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "sha256:1ipw8dns0cjcdbpy7s0vbjsfdf6c2943p1mijncmp7aq1flyyzsn";
  };
in import pkgs
