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

    # Temporary hacks (while upstream finishes doing these correctly)
    # ===============================================================
    tensorflow = callPackage ./pkgs/applications/libraries/tensorflow {
      pythonPackages = self.upstream-pkgs.pythonPackages;
    };

    # Packages from upstream
    # ======================
    scala = self.upstream-pkgs.scala;
    scala-2_10 = self.upstream-pkgs.scala_2_10;
    protobuf3_1 = self.upstream-pkgs.protobuf3_1;

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
                                  terraform-mode;
        inherit (emacsPackages) scalaMode2 erlangMode colorThemeSolarized;
        inherit (haskellPackages) hlint stylish-haskell;
        inherit (ocamlPackages_4_02) merlin ocpIndent utop;
        emacs-config-options = self.local-config.emacs-config;
      });

    # We need upstream emacs wrapper, since the one in the channel install stuff
    # in site-lisp, bin, etc
    emacsWithPackages = import ./build-support/emacs/wrapper.nix {
      inherit (pkgs) lib makeWrapper stdenv runCommand;
      inherit (pkgs.xorg) lndir;
    } pkgs.emacsPackagesNg;

    # aspell needs to be configured to find the dictionaries
    aspell-wrapped = callPackage ./pkgs/development/libraries/aspell-wrapped { };

    # Scala stuff
    # ===========
    scalacheck = callPackage ./pkgs/development/scala/scalacheck { };
    samtime = callPackage ./../src/scala/samtime/nix { };

    # Haskell stuff
    # =============
    name-generator = callPackage ./../src/haskell/name-generator/nix { };

    # Shell-scripts
    # =============
    assorted-scripts = callPackage ./../src/shell/assorted-scripts/nix {
      inherit (pkgs.xlibs) xbacklight xrandr xset;
    };

    # Experiments
    # ===========
    experiments-haskell = callPackage ./../src/experiments/haskell/nix { };
  };
in
self
