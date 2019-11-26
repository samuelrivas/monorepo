let pkgs = builtins.fetchGit {
      url = https://github.com/NixOS/nixpkgs.git;
      name = "nixpkgs-stable";
      ref = "nixos-19.09";
      rev = "4ad6f1404a8cd69a11f16edba09cc569e5012e42";
    };
in import pkgs
