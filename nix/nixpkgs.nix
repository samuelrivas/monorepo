let
  version = "a445f5829889959d65ad65e5c961d5c67e1cd677";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "0zl930jjacdphplw1wv5nlhjk15zvflzzwp53zbh0l8qq01wh7bl";
  };
in import pkgs
