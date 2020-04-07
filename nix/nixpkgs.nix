let pkgs = builtins.fetchGit {
      url = https://github.com/NixOS/nixpkgs.git;
      name = "nixpkgs-stable";
      ref = "nixos-20.03";
      rev = "0bb35152be895abfd1fc743b42f1c4e56ae71906";
    };
in import pkgs
