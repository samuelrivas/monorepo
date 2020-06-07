let pkgs = builtins.fetchGit {
      url = https://github.com/NixOS/nixpkgs.git;
      name = "nixpkgs-stable";
      ref = "nixos-20.03";
      rev = "1aa5271117107032e13f07bf025e3c4d26db8915";
    };
in import pkgs
