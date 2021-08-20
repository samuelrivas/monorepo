let
  version = "7bbca9877caed472c6b5866ea09302cfcdce3dbf";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "1byrw1inwrlw7yp5dwvdf0zv1zdqnjq32j1j7cmlwah4x7f46bvg";
  };
in import pkgs
