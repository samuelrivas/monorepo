let pkgs =  builtins.fetchGit {
      url = https://github.com/NixOS/nixpkgs.git;
      name = "nixpkgs-upstream";
      ref = "master";
      rev = "bb9da1323ebe39ff98a8412b963078a3a2bd2687";
    };
in import pkgs
