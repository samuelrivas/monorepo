# This is the root package set, it contains all packages from `nixpkgs`, and a
# few added by us. All packages we add are also part of the pkgs-sam subset. The
# rest of nix expressions are structured following these guidelines:
#
#  * For the sources contained in this monorepo, the nix expressions to build
#    them are colocated with the source and linked from here
#  * For the packages that use external sources (usually packages that are
#    patches to the official nix tree) we use nixpkgs-like struture inside pkgs

{ system ? builtins.currentSystem }:

let
  home-dir = builtins.getEnv "HOME";
  local-config-file = "${home-dir}/.local-nix-config/configuration.nix";
  pkgs = import ./nixpkgs.nix { inherit system; };
  pkgs-all = pkgs // pkgs-sam;
  builders = pkgs.callPackage ./lib/build-support/builders.nix { };
  callPackage = pkgs.lib.callPackageWith (pkgs-all // builders);
  pkgs-sam = {

    # Library functions
    # =================
    # Just so that we can use them when debugging in nix-repl
    inherit builders;
    inherit pkgs;

    # Config
    # ======

    # This is similar to nixos modules, but for the local user:
    #  * We read the file ${local-config-file} if it exists so that we can
    #    customise the build for different environments
    #  * We blend that with our ow configuration modules (using the same
    #    mechanisms as nixos)
    #  * The resulting config (local-config) can then be used by the derivations
    #    below
    #
    # Note that this is mainly for derivations that create configuration files,
    # which is a slight departure of what nixpkgs typically does
    local-config = import ./lib/load-config.nix {
      inherit (pkgs) lib;
      pkgs = pkgs-all;
      config-file = local-config-file;
      modules = [ ./modules/emacs-config.nix
                  ./modules/sams-pkgs.nix
                ];
    };

    # Master branch of the nixpkgs repo
    pkgs-upstream = import ./nixpkgs-upstream.nix { inherit system; };

    # My own fork of nixpkgs, for patches that aren't merged
    pkgs-patched = import ./nixpkgs-patched.nix { inherit system; };

    # Emacs stuff
    # ===========
    emacs-config = callPackage ./../src/elisp/emacs-config/nix
      (pkgs-sam.local-config.emacs-config // {
        inherit (pkgs) emacs;
      });

    # An emacs wrapper with the needed packages accessible
    emacs = callPackage ./pkgs/applications/editors/my-emacs
      (with pkgs; {
        inherit (emacsPackages)
          dumb-jump
          erlangMode
          flycheck-haskell
          groovy-mode
          haskell-mode
          helm
          helm-ls-git
          htmlize
          nix-mode
          projectile
          scalaMode2
          terraform-mode
          tuareg
          yaml-mode;
        inherit (pkgs-sam.pkgs-upstream.emacsPackages) colorThemeSolarized;
        inherit (pkgs-sam.pkgs-patched.haskellPackages) hlint stylish-haskell;
        inherit (ocamlPackages) merlin ocp-indent utop;
        emacs-config-options = pkgs-sam.local-config.emacs-config;
      });

    # A utility to instantiate a capable emacs in a haskell sandbox
    emacs-for-haskell = haskell-env: pkgs-sam.emacs.override { ghc = haskell-env; };

    # aspell needs to be configured to find the dictionaries
    aspell-wrapped = callPackage ./pkgs/development/libraries/aspell-wrapped { };

    # Haskell stuff
    # =============
    haskell-mk = callPackage ./../src/haskell/haskell-mk/nix {  };

    profiledHaskellPackages = pkgs.haskellPackages.override {
      overrides = pkgs-sam: super: {
        mkDerivation = args: super.mkDerivation (args // {
          enableLibraryProfiling = true;
        });
      };
    };

    haskell-pkg =
      { haskellPackages ? pkgs.haskellPackages,
        name,
        sandbox ? false,
        src,
        wanted-packages,
        extra-build-inputs ? [],
      } :
      let
        haskell-packages-selector = _: wanted-packages;
        ghc = haskellPackages.ghcWithPackages haskell-packages-selector;
      in
        pkgs.stdenv.mkDerivation rec {

          inherit name src;

          buildInputs = [
            ghc
            pkgs-sam.haskell-mk
          ]
          ++ extra-build-inputs
          ++ (if sandbox
              then [(pkgs-sam.emacs-for-haskell ghc) haskellPackages.hoogle]
              else []);

          installPhase = ''
            mkdir -p $out/bin
            cp ../build/bin/* $out/bin
          '';
        };

    name-generator = callPackage ./../src/haskell/name-generator/nix {
      inherit (pkgs-sam.pkgs-patched) haskellPackages;
      sandbox = false;
    };
    name-generator-sandbox = callPackage ./../src/haskell/name-generator/nix {
      inherit (pkgs-sam.pkgs-patched) haskellPackages;
      sandbox = true;
    };
    ds-processing = callPackage ./../src/haskell/ds-processing/nix {
      inherit (pkgs-sam.pkgs-patched) haskellPackages;
      sandbox = false;
    };
    boardgamer = callPackage ./../src/haskell/boardgamer/nix {
      inherit (pkgs-sam.pkgs-patched) haskellPackages;
      sandbox = false;
    };
    boardgamer-sandbox = callPackage ./../src/haskell/boardgamer/nix {
      inherit (pkgs-sam.pkgs-patched) haskellPackages;
      sandbox = true;
    };
    hashcode-photoalbum-sandbox = callPackage ./../src/haskell/hashcode-photoalbum/nix {
      inherit (pkgs-sam.pkgs-patched) haskellPackages;
      # haskellPackages = pkgs-sam.profiledHaskellPackages;
      sandbox = true;
    };
    hashcode-photoalbum = callPackage ./../src/haskell/hashcode-photoalbum/nix {
      inherit (pkgs-sam.pkgs-patched) haskellPackages;
      sandbox = false;
    };
    onirim-helper-sandbox = callPackage ./../src/haskell/onirim-helper/nix {
      inherit (pkgs-sam.pkgs-patched) haskellPackages;
      # haskellPackages = pkgs-sam.profiledHaskellPackages;
      sandbox = true;
    };
    onirim-helper = callPackage ./../src/haskell/onirim-helper/nix {
      inherit (pkgs-sam.pkgs-patched) haskellPackages;
      sandbox = false;
    };
    low-battery-sandbox = callPackage ./../src/haskell/low-battery/nix {
      inherit (pkgs-sam.pkgs-patched) haskellPackages;
      sandbox = true;
    };
    low-battery = callPackage ./../src/haskell/low-battery/nix {
      inherit (pkgs-sam.pkgs-patched) haskellPackages;
      sandbox = false;
    };

    # Shell-scripts
    # =============
    assorted-scripts = callPackage ./../src/shell/assorted-scripts/nix {
      inherit (pkgs.xlibs) xbacklight xrandr xset;
    };

    sh-lib = callPackage ./../src/shell/sh-lib/nix { };

    sandbox = callPackage ./../src/shell/sandbox/nix {
      nix-root = pkgs-sam.local-config.sams-pkgs.dir + "/default.nix";
    };

    commit-hook-ticket-prefix = callPackage ./../src/shell/commit-hook-ticket-prefix/nix { };

    # C++ stuff
    # =========
    reservoir = callPackage ./../src/c++/reservoir/nix {
      sandbox = false;
    };
    reservoir-sandbox = callPackage ./../src/c++/reservoir/nix {
      sandbox = true;
    };
    monte-carlo = callPackage ./../src/c++/monte-carlo/nix {
      sandbox = false;
    };
    monte-carlo-sandbox = callPackage ./../src/c++/monte-carlo/nix {
      sandbox = true;
    };
    algos-n-fun = callPackage ./../src/c++/algos-n-fun/nix {
      inherit (pkgs-sam.pkgs-upstream) rapidcheck;
    };
    finndb = callPackage ./../src/c++/finndb/nix {
      sandbox = false;
    };
    finndb-sandbox = callPackage ./../src/c++/finndb/nix {
      sandbox = true;
    };
    graphlib = callPackage ./../src/c++/graphlib/nix { };
    rndlib = callPackage ./../src/c++/rndlib/nix { };
    asyncq = callPackage ./../src/c++/asyncq/nix { };

    # C stuff
    # =======
    udp-cat = callPackage ./pkgs/applications/networking/tools/udp-cat { };

    # Contests, puzzles, etc
    # ======================
    adventofcode-2019-sandbox = callPackage ./../src/puzzles/adventofcode/2019/nix {
      inherit (pkgs-sam.pkgs-patched) haskellPackages;
      sandbox = true;
    };
  };
in
# All official packages plus ours. We also add pkgs-sam as a set with all our
# packages so that we can run nix-build -A pkgs-sam and test this monorepo
pkgs-all // { inherit pkgs-sam; }
