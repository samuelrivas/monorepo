# We load this function in flake.nix to overlay all our packages over the
# nixpkgs collection.
#
# Note that this is incomplete, several packages are only available via the
# legacy nix/default.nix. We'll keep migrating those to here and eventually
# deprecate nix/default.nix.
final: prev:
let
  builders = final.callPackage ./lib/build-support/builders.nix { };
  derivation-helpers = import ./lib/derivation-helpers.nix;
  libs-sam = { inherit builders derivation-helpers; };
  callPackage = final.lib.callPackageWith
    (final.pkgs // builders // derivation-helpers);
  pkgs = {

    # We keep our libraries in a separate attrset and pass them with
    # callPackageWith instead of polluting the top level namespace with them. We
    # do add all our pakcages to the top level namespace though
    inherit libs-sam;

    derivations-sam = {
      # Emacs stuff
      # =============
      emacs-config = callPackage ./../src/elisp/emacs-config/nix {
        full-user-name = "";
        extra-config = "";
      };

      # An emacs wrapper with the needed packages accessible
      #
      # FIXME This does not work in the flake, we still need to have a clean way
      # to feed in configuration, so this is just here so that it builds, but
      # not everything works as instended. The idea is to mvoe this out of the
      # monorepo into a private one to avoid poluting this with personal
      # configuration.
      my-emacs = callPackage ./pkgs/applications/editors/my-emacs {
        inherit (final.emacsPackages)
          colorThemeSolarized
          company
          eglot
          erlangMode
          flycheck-haskell
          groovy-mode
          haskell-mode
          helm
          helm-ls-git
          helm-org
          htmlize
          markdown-mode
          nix-mode
          projectile
          terraform-mode
          yaml-mode
          yasnippet;
        emacs-config-options.denylisted-modes = [ "erlang" "ocaml" ];
      };

      # aspell needs to be configured to find the dictionaries
      aspell-wrapped =
        callPackage ./pkgs/development/libraries/aspell-wrapped { };

      # Shell-scripts
      # =============
      assorted-scripts = callPackage ./../src/shell/assorted-scripts/nix {
        inherit (final.xorg) xbacklight xrandr xset;
      };

      sh-lib = callPackage ./../src/shell/sh-lib/nix { };

      commit-hook-ticket-prefix =
        callPackage ./../src/shell/commit-hook-ticket-prefix/nix { };

      # C++ stuff
      # =========
      reservoir = callPackage ./../src/c++/reservoir/nix { };

      monte-carlo = callPackage ./../src/c++/monte-carlo/nix { };
      algos-n-fun = callPackage ./../src/c++/algos-n-fun/nix { };
      finndb = callPackage ./../src/c++/finndb/nix { };
      graphlib = callPackage ./../src/c++/graphlib/nix { };
      rndlib = callPackage ./../src/c++/rndlib/nix { };
      asyncq = callPackage ./../src/c++/asyncq/nix { };

      udp-cat = final.callPackage ./pkgs/applications/networking/tools/udp-cat { };
    };
    all-pkgs-sam = final.symlinkJoin {
      name = "pkgs-sam";
      paths = builtins.attrValues pkgs.derivations-sam;
    };
  };
in pkgs // pkgs.derivations-sam // {
  # Add all-pkgs-sam here to the set with all derivations to avoid infinite
  # recursion
  derivations-sam = pkgs.derivations-sam // { inherit (pkgs) all-pkgs-sam; };
}

