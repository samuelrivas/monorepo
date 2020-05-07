let pkgs = builtins.fetchGit {
      url = https://github.com/NixOS/nixpkgs.git;
      name = "nixpkgs-stable";
      ref = "nixos-20.03";
      rev = "4d373182597cff60b3a820affb5a73dd274e205b";
    };
in import pkgs
