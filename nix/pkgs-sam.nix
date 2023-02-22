# We load this function in flake.nix to overlay all our packages over the
# nixpkgs collection.
#
# Note that this is incomplete, several packages are only available via the
# legacy nix/default.nix. We'll keep migrating those to here and eventually
# deprecate nix/default.nix.
nixpkgs:
let
  builders = nixpkgs.callPackage ./lib/build-support/builders.nix { };
  derivation-helpers = import ./lib/derivation-helpers.nix;
  callPackage = nixpkgs.lib.callPackageWith
    (nixpkgs // builders // derivation-helpers // pkgs-sam);
  pkgs-sam = {

    # Emacs stuff
    # =============
    # aspell needs to be configured to find the dictionaries
    aspell-wrapped =
      callPackage ./pkgs/development/libraries/aspell-wrapped { };

    # Shell-scripts
    # =============
    assorted-scripts = callPackage ./../src/shell/assorted-scripts/nix {
      inherit (nixpkgs.xorg) xbacklight xrandr xset;
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

    udp-cat = callPackage ./pkgs/applications/networking/tools/udp-cat { };
  };
in pkgs-sam
