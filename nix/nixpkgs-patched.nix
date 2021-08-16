let
  version = "06770322e46bd87cd69496932c8b42739c3c539d";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/samuelrivas/nixpkgs/archive/${version}.tar.gz";
    sha256 = "1jhbcaj6l4aqb4mwm0gn9982f8b41mxkqjyk6g26hz8g3nvvapl1";
  };
in import pkgs
