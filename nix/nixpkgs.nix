let pkgs = builtins.fetchGit {
      url = https://github.com/NixOS/nixpkgs.git;
      name = "nixpkgs-stable";
      ref = "nixos-20.03";
      rev = "9f0f06ac8beb8852d7ab3c5732e1c008c312f38d";
    };
in import pkgs
