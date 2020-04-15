let pkgs = builtins.fetchGit {
      url = https://github.com/samuelrivas/nixpkgs.git;
      name = "nixpkgs-patched";
      ref = "HsYAML-0-2";
      rev = "fed961a02890a41c264ae85f04b3b636c82949fb";
    };
in import pkgs
