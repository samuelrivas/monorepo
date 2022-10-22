# Reference to the latest nixos-22.05
let
  version = "ac20a8605b0f79be2d65d995cd347251cd5b984b";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "sha256:184asc3j4vg0qys10yqlla5h05wfb3jm062lwzsrlnjw2yq7nll3";
  };
in import pkgs
