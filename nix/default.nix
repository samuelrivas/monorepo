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

    # Scala stuff
    # ===========
    scalacheck = callPackage ./pkgs/development/scala/scalacheck { };

    # Old stuff cowardly kept here, delete when you are tired of seeing it
    # ====================================================================
    spark_1_4_0 = callPackage ./pkgs/applications/networking/cluster/spark/1.4.0.nix { };
  };
in
self
