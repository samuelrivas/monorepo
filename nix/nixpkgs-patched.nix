let pkgs = builtins.fetchGit {
      url = https://github.com/samuelrivas/nixpkgs.git;
      name = "nixpkgs-patched";
      ref = "fix-color-theme-solarized";
      rev = "fdb00fdc2c146171e424bbe9263780462bb7964c";
    };
in import pkgs
