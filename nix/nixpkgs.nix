let
  version = "1f91fd1040667e9265a760b0347f8bc416249da7";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "1lcfcwgal9fpaiq71981abyzz160r6nx1y4pyy1dnvaf951xkdcj";
  };
in import pkgs
