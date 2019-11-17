let pkgs = builtins.fetchGit {
      url = https://github.com/NixOS/nixpkgs.git;
      name = "nxipkgs-stable";
      ref = "nixos-19.09";
      rev = "c0c062da7b3e20ec3b6c9416bc03cb3fc61df4b8";
    };
in import pkgs
