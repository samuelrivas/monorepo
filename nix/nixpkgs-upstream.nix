let pkgs =  builtins.fetchGit {
      url = https://github.com/NixOS/nixpkgs.git;
      name = "nxipkgs-upstream";
      ref = "master";
      rev = "f5670ebd8e793aeb599c8f49794b3dafc31c27fd";
    };
in import pkgs
