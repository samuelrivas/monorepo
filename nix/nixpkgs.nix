let pkgs = builtins.fetchGit {
      url = https://github.com/NixOS/nixpkgs.git;
      name = "nixpkgs-stable";
      ref = "nixos-20.03";
      rev = "a53ed231d21803b61ca3cb10cedef9b7c09a2503";
    };
in import pkgs
