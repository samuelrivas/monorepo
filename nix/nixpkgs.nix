# Reference to the latest nixos-22.05
let
  version = "96e18717904dfedcd884541e5a92bf9ff632cf39";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "sha256:0zw1851mia86xqxdf8jgy1c6fm5lqw4rncv7v2lwxar3vhpn6c78";
  };
in import pkgs
