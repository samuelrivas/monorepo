let
  version = "aa2f845096f72dde4ad0c168eeec387cbd2eae04";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "sha256:0l732ci2g78pcgk9kqn6c18h4j47dhp1dys52cmqhzm4pyi6dl0z";
  };
in import pkgs
