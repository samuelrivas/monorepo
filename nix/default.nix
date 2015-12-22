# A few remarks
#
#  * This structure is probably going to change
#  * I don't like camel case, so I won't use it for my stuff, even at the cost
#    of needing to rewrite when contributing

{ system ? builtins.currentSystem }:

let
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

    # Patches from upstream, to be pull requested
    # ===========================================
    java-json = callPackage ./pkgs/development/java/json { };
    java-mailapi = callPackage ./pkgs/development/java/mailapi { };
    spark = callPackage ./pkgs/applications/networking/cluster/spark { };

    # Patches not yet in channels, but merged upstream
    # These should go away soon
    # ================================================
    scala = callPackage ./pkgs/development/compilers/scala { };
    color-theme-solarized = callPackage ./pkgs/applications/editors/emacs-modes/color-theme-solarized {
      inherit (pkgs.emacs24Packages) colorTheme;
    };
    erlang-mode = callPackage ./pkgs/applications/editors/emacs-modes/erlang-mode { };

    # Emacs stuff
    # ===========

    # TODO:
    #  * Read the config for this from a file a-la configuration.nix
    #  * Make this modular, so we don't need to install the world just to get
    #    one mode configured
    emacs-config = callPackage ./config/emacs {
      inherit (pkgs.emacsPackages) haskellMode tuaregMode scalaMode2;
      inherit (pkgs.ocamlPackages_4_02_1) merlin ocpIndent utop;

      user = "samuel";
      full-user-name = "Samuel Rivas";
      extra-config = ''
        ;; workarounds
        (require 'iso-transl) ; required for dead keys to work with ibus
      '';
    };

    # Scala stuff
    # ===========
    scalacheck = callPackage ./pkgs/development/scala/scalacheck { };

    # Old stuff cowardly kept here, delete when you are tired of seeing it
    # ====================================================================
    spark_1_4_0 = callPackage ./pkgs/applications/networking/cluster/spark/1.4.0.nix { };
  };
in
self
