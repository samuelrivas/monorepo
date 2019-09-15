# A few remarks
#
#  * This structure is probably going to change
#  * I don't like camel case, so I won't use it for my stuff, even at the cost
#    of needing to rewrite when contributing
#
# Right now, the high level structure is:
#
#  * For the sources contained in this monorepo, the nix expressions to build
#    them are colocated with the source and linked from here
#  * For the packages that use external sources (usually packages that are
#    patches to the official nix tree) we use nixpkgs-like struture inside pkgs

{ system ? builtins.currentSystem }:

let
  home-dir = builtins.getEnv "HOME";
  local-config-file = "${home-dir}/.local-nix-config/configuration.nix";
  pkgs = import <nixpkgs> { inherit system; };
  builders = pkgs.callPackage ./lib/build-support/builders.nix { };
  callPackage = pkgs.lib.callPackageWith (pkgs
                                       // builders
                                       // self);
  self = {

    # Library functions
    # =================
    # Just so that we can use them when debugging in nix-repl
    inherit builders;

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
      pkgs = pkgs // self;
      config-file = local-config-file;
      modules = [ ./modules/emacs-config.nix
                  ./modules/upstream-pkgs.nix
                  ./modules/sams-pkgs.nix
                ];
    };

    upstream-pkgs = import (self.local-config.upstream-pkgs.dir)  { inherit system; };

    # Own packages, not general enough
    # ================================
    packer = callPackage ./pkgs/development/tools/packer { };

    # Patches to upstream, to be pull requested
    # ===========================================
    java-json = callPackage ./pkgs/development/java/json { };
    java-mailapi = callPackage ./pkgs/development/java/mailapi { };
    spark = callPackage ./pkgs/applications/networking/cluster/spark {
      mesosSupport = false;
    };

    cpplint = callPackage ./pkgs/development/tools/cpplint { };

    convox = callPackage ./pkgs/development/tools/convox { };

    # Packages from upstream
    # ======================
    scala = self.upstream-pkgs.scala;
    scala-2_10 = self.upstream-pkgs.scala_2_10;

    # Trivial sandboxes
    # =================
    bazel-sandbox = callPackage ./pkgs/development/tools/bazel-sandbox {
      inherit (self.upstream-pkgs) bazel;
    };

    # Emacs stuff
    # ===========
    emacs-config = callPackage ./../src/elisp/emacs-config/nix
      (self.local-config.emacs-config // {
        inherit (pkgs) emacs;
      });

    # An emacs wrapper with the needed packages accessible
    emacs = callPackage ./pkgs/applications/editors/my-emacs
      (with pkgs; {
        inherit (emacsPackagesNg) flycheck-haskell haskell-mode
                                  nix-mode groovy-mode tuareg
                                  terraform-mode yaml-mode;
        inherit (emacsPackages) scalaMode2 erlangMode;
        inherit (self.upstream-pkgs.emacsPackages) colorThemeSolarized;
        inherit (haskellPackages) hlint stylish-haskell;
        inherit (ocamlPackages_4_03) merlin ocp-indent utop;
        emacs-config-options = self.local-config.emacs-config;
      });

    # A utility to instantiate a capable emacs in a haskell sandbox
    emacs-for-haskell = haskell-env: self.emacs.override { ghc = haskell-env; };

    # aspell needs to be configured to find the dictionaries
    aspell-wrapped = callPackage ./pkgs/development/libraries/aspell-wrapped { };

    # Scala stuff
    # ===========
    scalacheck = callPackage ./pkgs/development/scala/scalacheck { };
    samtime = callPackage ./../src/scala/samtime/nix { };

    # Haskell stuff
    # =============
    profiledHaskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: {
        mkDerivation = args: super.mkDerivation (args // {
          enableLibraryProfiling = true;
        });
      };
    };

    name-generator = callPackage ./../src/haskell/name-generator/nix {
      sandbox = false;
    };
    name-generator-sandbox = callPackage ./../src/haskell/name-generator/nix {
      sandbox = true;
    };
    ds-processing = callPackage ./../src/haskell/ds-processing/nix {
      sandbox = false;
    };
    boardgamer = callPackage ./../src/haskell/boardgamer/nix {
      sandbox = false;
    };
    boardgamer-sandbox = callPackage ./../src/haskell/boardgamer/nix {
      sandbox = true;
    };
    hashcode-photoalbum-sandbox = callPackage ./../src/haskell/hashcode-photoalbum/nix {
      haskellPackages = self.profiledHaskellPackages;
      sandbox = true;
    };
    hashcode-photoalbum = callPackage ./../src/haskell/hashcode-photoalbum/nix {
      sandbox = false;
    };

    # Shell-scripts
    # =============
    assorted-scripts = callPackage ./../src/shell/assorted-scripts/nix {
      inherit (pkgs.xlibs) xbacklight xrandr xset;
    };

    sh-lib = callPackage ./../src/shell/sh-lib/nix { };

    sandbox = callPackage ./../src/shell/sandbox/nix {
      nix-root = self.local-config.sams-pkgs.dir + "/default.nix";
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
      inherit (self.upstream-pkgs) rapidcheck;
    };
    finndb = callPackage ./../src/c++/finndb/nix {
      inherit (pkgs.python3Packages) csvkit;
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

    # Python stuff
    # ============
    python36Packages = callPackage ./python-packages.nix {
      pythonPackages = pkgs.python36Packages;
    };
  };
in
self
