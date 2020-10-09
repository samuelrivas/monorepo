let
  version = "0d0660fde3bb53a3d013b65e5e141eb11d1efb82";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "13qpa916qq1kqvfj8q4zkmnfnbh2kpx0nxxg04nblai0smz97820";
  };
in import pkgs
