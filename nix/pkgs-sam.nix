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

    haskell-lib = import ./lib/haskell.nix {
      inherit (final) pkgs;
      inherit (final.pkgs) haskell-test-mk haskell-mk haskell-lib-mk;
      emacs = pkgs.my-emacs;
    };

    haskellPackages-new = prev.haskellPackages.override {
      overrides = h-final: h-prev: {
        mk-conf-file = callPackage ./../src/haskell/mk-conf-file/nix {
          inherit (final.haskell-lib) haskell-pkg;
        };
        perlude = callPackage ./../src/haskell/perlude/nix {
          inherit (final.haskell-lib) haskell-lib-pkg;
        };
        boollib = callPackage ./../src/haskell/boollib/nix {
          inherit (final.haskell-lib) haskell-lib-pkg;
        };
        parselib = callPackage ./../src/haskell/parselib/nix {
          inherit (final.haskell-lib) haskell-lib-pkg;
          haskellPackages = final.haskellPackages-new;
        };
        clean-clocks = callPackage ./../src/haskell/clean-clocks/nix {
          inherit (final.haskell-lib) haskell-pkg;
          haskellPackages = final.haskellPackages-new;
        };
      };
    };

    # haskellPackages = final.haskell-lib.mk-haskell-packages
    #   prev.haskellPackages final.samsHaskellPackagesGen;

    # haskellPackagesPatched = pkgs.haskell-lib.mk-haskell-packages
    #   pkgs.pkgs-patched.haskellPackages pkgs.samsHaskellPackagesGen;

    ## For some reason I need to explicitly pass haskellPackages here, otherwise
    ## the derivations get the version before overriding (i.e. the one without
    ## my packages)
    # samsHaskellPackagesGen = hp: builtins.mapAttrs
    #   (name: path: hp.callPackage path { haskellPackages = hp; })
    #   {
    #     adventlib-old-1 = ./../src/haskell/adventlib-old-1/nix;
    #     adventlib = ./../src/haskell/adventlib/nix;
    #     adventofcode-2019 = ./../src/puzzles/adventofcode/2019/nix;
    #     adventofcode-2020 = ./../src/puzzles/adventofcode/2020/nix;
    #     adventofcode-2021 = ./../src/puzzles/adventofcode/2021/nix;
    #     boardgamer = ./../src/haskell/boardgamer/nix;
    #     boollib = ./../src/haskell/boollib/nix;
    #     clean-clocks = ./../src/haskell/clean-clocks/nix;
    #     example-lib =  ./../src/haskell/example-lib/nix;
    #     hashcode-photoalbum =  ./../src/haskell/hashcode-photoalbum/nix;
    #     low-battery = ./../src/haskell/low-battery/nix;
    #     mk-conf-file = ./../src/haskell/mk-conf-file/nix;
    #     monad-emit = ./../src/haskell/monad-emit/nix;
    #     name-generator =  ./../src/haskell/name-generator/nix;
    #     onirim-helper = ./../src/haskell/onirim-helper/nix;
    #     parselib = ./../src/haskell/parselib/nix;
    #     perlude = ./../src/haskell/perlude/nix;
    #     searchlib = ./../src/haskell/searchlib/nix;
    #   };

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

      # Haskell stuff
      # =============

      # FIXME: haskell-mk (and maybe emacs) should be passed as arguments to
      # the derivations, currently we are implicitly adding them as dependencies
      # because they are part of haskell-lib. The problem with the current setting
      # is that we cannot change any of them without affecting all our haskell
      # packages.
      #
      # haskell-lib-mk cannot be added to haskell-pkg as mk-conf-file uses
      # haskell-pkg

      haskell-mk = callPackage ./../src/haskell/haskell-mk/nix {  };
      haskell-lib-mk = callPackage ./../src/haskell/haskell-lib-mk/nix {  };
      haskell-test-mk = callPackage ./../src/haskell/haskell-test-mk/nix {  };

      # adventlib = final.haskellPackages.adventlib;
      # adventlib-old-1 = final.haskellPackages.adventlib;
      # boardgamer = final.haskellPackages.boardgamer;
      # boollib = final.haskellPackages.boollib;
      # clean-clocks = final.haskellPackages.clean-clocks;
      # hashcode-photoalbum = final.haskellPackages.hashcode-photoalbum;
      # low-battery = final.haskellPackages.low-battery;
      # mk-conf-file = final.haskellPackages.mk-conf-file;
      # monad-emit = final.haskellPackages.monad-emit;
      # name-generator = final.haskellPackages.name-generator;
      # onirim-helper = final.haskellPackages.onirim-helper;
      # parselib = final.haskellPackages.parselib;
      # perlude = final.haskellPackages.perlude;
      # searchlib = final.haskellPackages.searchlib;

      mk-conf-file = final.haskellPackages-new.mk-conf-file;
      perlude = final.haskellPackages-new.perlude;
      boollib = final.haskellPackages-new.boollib;
      parselib = final.haskellPackages-new.parselib;
      clean-clocks = final.haskellPackages-new.clean-clocks;

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

      # Contests, puzzles, etc
      # ======================
      adventofcode-2019 = final.haskellPackages.adventofcode-2019;
      adventofcode-2020 = final.haskellPackages.adventofcode-2020;
      adventofcode-2021 = final.haskellPackages.adventofcode-2021;
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
