# A few remarks
#
#  * This structure is probably going to change
#  * I don't like camel case, so I won't use it for my stuff, even at the cost
#    of needing to rewrite when contributing

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
      modules = [ ./modules/emacs-config.nix ];
    };

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
    #  * Make this modular, so we don't need to install the world just to get
    #    one mode configured
    emacs-config = callPackage ./pkgs/applications/editors/emacs-config
      (self.local-config.emacs-config // {
         inherit (pkgs.emacsPackages) haskellMode tuaregMode scalaMode2;
         inherit (self.ocamlPackages_4_02) merlin ocpIndent utop;
      });

    # Scala stuff
    # ===========
    scalacheck = callPackage ./pkgs/development/scala/scalacheck { };

    # Ocaml stuff
    # ===========

    # Stolen from upstream, as 4.02.1 is broken in the channel right now
    ocaml_4_02 = callPackage ./pkgs/development/compilers/ocaml/4.02.nix {
      inherit (pkgs.xorg) libX11 xproto;
    };
    ocamlPackages_4_02 = pkgs.mkOcamlPackages self.ocaml_4_02 self.ocamlPackages_4_02;


    # Old stuff cowardly kept here, delete when you are tired of seeing it
    # ====================================================================
    spark_1_4_0 = callPackage ./pkgs/applications/networking/cluster/spark/1.4.0.nix { };
  };
in
self
