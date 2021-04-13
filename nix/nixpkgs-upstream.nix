let
  version = "c3253cdab424b87ad8490d42f6585302b20bd39b";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "16dx12n4fnl5r4k1bbam5fp5bym120g20k675m4xdc9280r0liq7";
  };
in import pkgs
