# Change to the faster fetchTarball when you modify this
let pkgs = builtins.fetchGit {
      url = https://github.com/samuelrivas/nixpkgs.git;
      name = "nixpkgs-patched";
      ref = "HsYAML-0-2";
      rev = "942389f161d521e8a606aead31970d089ca2a0e6";
    };
in import pkgs
