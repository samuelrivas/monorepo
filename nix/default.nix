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
  derivation-helpers = import ./lib/derivation-helpers.nix;
  callPackage = pkgs.lib.callPackageWith
    (pkgs-all // builders // derivation-helpers);
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
    emacs-config = callPackage ./../src/elisp/emacs-config/nix {
      inherit (pkgs-sam.local-config.emacs-config)
        full-user-name
        extra-config;
      inherit (pkgs) emacs;
    };

    # This is where we can override failing packages
    emacsPackages = pkgs.emacsPackages.overrideScope' (
      self: super:
      {
      });

    # An emacs wrapper with the needed packages accessible
    emacs = callPackage ./pkgs/applications/editors/my-emacs
      (with pkgs; {
        inherit (pkgs-sam.emacsPackages)
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
        emacs-config-options = pkgs-sam.local-config.emacs-config;
      });

    # aspell needs to be configured to find the dictionaries
    aspell-wrapped = callPackage ./pkgs/development/libraries/aspell-wrapped { };

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
    haskell-lib = import ./lib/haskell.nix {
      inherit pkgs;
      inherit (pkgs-sam) emacs haskell-mk haskell-lib-mk haskell-test-mk;
    };

    haskellPackages = pkgs-sam.haskell-lib.mk-haskell-packages
      pkgs.haskellPackages pkgs-sam.samsHaskellPackagesGen;

    haskellPackagesPatched = pkgs-sam.haskell-lib.mk-haskell-packages
      pkgs-sam.pkgs-patched.haskellPackages pkgs-sam.samsHaskellPackagesGen;

    ## For some reason I need to explicitly pass haskellPackages here, otherwise
    ## the derivations get the version before overriding (i.e. the one without
    ## my packages)
    samsHaskellPackagesGen = hp: builtins.mapAttrs
      (name: path: hp.callPackage path { haskellPackages = hp; })
      {
        adventlib = ./../src/haskell/adventlib/nix;
        adventofcode-2019 = ./../src/puzzles/adventofcode/2019/nix;
        adventofcode-2020 = ./../src/puzzles/adventofcode/2020/nix;
        adventofcode-2021 = ./../src/puzzles/adventofcode/2021/nix;
        boardgamer = ./../src/haskell/boardgamer/nix;
        boollib = ./../src/haskell/boollib/nix;
        clean-clocks = ./../src/haskell/clean-clocks/nix;
        example-lib =  ./../src/haskell/example-lib/nix;
        hashcode-photoalbum =  ./../src/haskell/hashcode-photoalbum/nix;
        low-battery = ./../src/haskell/low-battery/nix;
        mk-conf-file = ./../src/haskell/mk-conf-file/nix;
        monad-emit = ./../src/haskell/monad-emit/nix;
        name-generator =  ./../src/haskell/name-generator/nix;
        onirim-helper = ./../src/haskell/onirim-helper/nix;
        parselib = ./../src/haskell/parselib/nix;
        perlude = ./../src/haskell/perlude/nix;
        searchlib = ./../src/haskell/searchlib/nix;
      };

    # We need to run an onder version of random fu for some applications, so
    # keeping them at haskellPackagesPatched while we don't update them to
    # compile with the latest verison

    adventlib = pkgs-sam.haskellPackages.adventlib;
    boardgamer = pkgs-sam.haskellPackages.boardgamer;
    boollib = pkgs-sam.haskellPackages.boollib;
    clean-clocks = pkgs-sam.haskellPackages.clean-clocks;
    hashcode-photoalbum = pkgs-sam.haskellPackagesPatched.hashcode-photoalbum;
    low-battery = pkgs-sam.haskellPackages.low-battery;
    mk-conf-file = pkgs-sam.haskellPackages.mk-conf-file;
    monad-emit = pkgs-sam.haskellPackages.monad-emit;
    name-generator = pkgs-sam.haskellPackagesPatched.name-generator;
    onirim-helper = pkgs-sam.haskellPackagesPatched.onirim-helper;
    parselib = pkgs-sam.haskellPackages.parselib;
    perlude = pkgs-sam.haskellPackages.perlude;
    searchlib = pkgs-sam.haskellPackages.searchlib;

    # Shell-scripts
    # =============
    assorted-scripts = callPackage ./../src/shell/assorted-scripts/nix {
      inherit (pkgs.xorg) xbacklight xrandr xset;
    };

    sh-lib = callPackage ./../src/shell/sh-lib/nix { };

    sandbox = callPackage ./../src/shell/sandbox/nix {
      nix-root = pkgs-sam.local-config.sams-pkgs.dir + "/default.nix";
    };

    commit-hook-ticket-prefix = callPackage ./../src/shell/commit-hook-ticket-prefix/nix { };

    # C++ stuff
    # =========
    reservoir = callPackage ./../src/c++/reservoir/nix { };

    monte-carlo = callPackage ./../src/c++/monte-carlo/nix { };
    algos-n-fun = callPackage ./../src/c++/algos-n-fun/nix {
      inherit (pkgs-sam.pkgs-upstream) rapidcheck;
    };
    finndb = callPackage ./../src/c++/finndb/nix { };
    graphlib = callPackage ./../src/c++/graphlib/nix { };
    rndlib = callPackage ./../src/c++/rndlib/nix { };
    asyncq = callPackage ./../src/c++/asyncq/nix { };

    # C stuff
    # =======
    udp-cat = callPackage ./pkgs/applications/networking/tools/udp-cat { };

    # Latex base
    # ==========
    latex-base = callPackage ./../src/docs/latex-base/nix {
      inherit (pkgs.lua53Packages) digestif;
      inherit (pkgs-sam) emacs;
    };

    # Contests, puzzles, etc
    # ======================
    adventofcode-2019 = pkgs-sam.haskellPackagesPatched.adventofcode-2019;
    adventofcode-2020 = pkgs-sam.haskellPackagesPatched.adventofcode-2020;
    adventofcode-2021 = pkgs-sam.haskellPackagesPatched.adventofcode-2021;
  };
in
# All official packages plus ours. We also add pkgs-sam as a set with all our
# packages so that we can run nix-build -A pkgs-sam and test this monorepo
pkgs-all // { inherit pkgs-sam; }
