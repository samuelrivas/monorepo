let pkgs = builtins.fetchGit {
      url = https://github.com/NixOS/nixpkgs.git;
      name = "nixpkgs-stable";
      ref = "nixos-19.09";
      rev = "d5291756487d70bc336e33512a9baf9fa1788faf";
    };
in import pkgs
