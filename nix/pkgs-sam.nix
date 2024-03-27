# We load this function in flake.nix to overlay all our packages over the
# nixpkgs collection.
#
# Note that this is incomplete, several packages are only available via the
# legacy nix/default.nix. We'll keep migrating those to here and eventually
# deprecate nix/default.nix.
final: prev: let
  sam-lib = import ./lib.nix;
  builders = final.callPackage ./lib/build-support/builders.nix {};
  derivation-helpers = sam-lib.legacy.derivation-helpers;
  libs-sam = {inherit builders derivation-helpers;};
  system-lib = import ./system-lib.nix {inherit (final) pkgs;};
  callPackage =
    final.lib.callPackageWith
    (final.pkgs // builders // derivation-helpers);
  pkgs = {
    # We keep our libraries in a separate attrset and pass them with
    # callPackageWith instead of polluting the top level namespace with them. We
    # do add all our pakcages to the top level namespace though
    inherit libs-sam system-lib;

    sam.system-lib = system-lib;
    haskell-lib = system-lib.haskell;

    haskellPackages = prev.haskellPackages.override {
      overrides = h-final: h-prev: let
        h-package = h-final.callPackage;
      in {
        inherit (final.haskell-lib) haskell-pkg haskell-lib-pkg;

        adventlib = h-package ./../src/haskell/adventlib/nix {};
        adventlib-old-1 = h-package ./../src/haskell/adventlib-old-1/nix {};
        adventofcode-2019 = h-package ./../src/puzzles/adventofcode/2019/nix {};
        adventofcode-2020 = h-package ./../src/puzzles/adventofcode/2020/nix {};
        adventofcode-2021 = h-package ./../src/puzzles/adventofcode/2021/nix {};
        boardgamer = h-package ./../src/haskell/boardgamer/nix {};
        boollib = h-package ./../src/haskell/boollib/nix {};
        clean-clocks = h-package ./../src/haskell/clean-clocks/nix {};
        example-lib = h-package ./../src/haskell/example-lib/nix {};
        hashcode-photoalbum = h-package ./../src/haskell/hashcode-photoalbum/nix {};
        low-battery = h-package ./../src/haskell/low-battery/nix {};
        mk-conf-file = h-package ./../src/haskell/mk-conf-file/nix {};
        monad-emit = h-package ./../src/haskell/monad-emit/nix {};
        name-generator = h-package ./../src/haskell/name-generator/nix {};
        onirim-helper = h-package ./../src/haskell/onirim-helper/nix {};
        parselib = h-package ./../src/haskell/parselib/nix {};
        perlude = h-package ./../src/haskell/perlude/nix {};
        searchlib = h-package ./../src/haskell/searchlib/nix {};
      };
    };

    derivations-sam = {
      # Emacs stuff
      # =============
      emacs-config = callPackage ./../src/elisp/emacs-config/nix {};

      copilot = callPackage ./pkgs/applications/editors/copilot {
        inherit
          (final.emacsPackages)
          dash
          editorconfig
          s
          trivialBuild
          ;
      };

      # An emacs wrapper with the needed packages accessible
      my-emacs = callPackage ./pkgs/applications/editors/my-emacs {
        inherit
          (final.emacsPackages)
          colorThemeSolarized
          company
          eglot
          flycheck-haskell
          groovy-mode
          helm
          helm-ls-git
          helm-org
          htmlize
          markdown-mode
          nix-mode
          projectile
          terraform-mode
          yaml-mode
          yasnippet
          ;
        inherit (final.python3Packages) jedi-language-server;
      };

      # aspell needs to be configured to find the dictionaries
      aspell-wrapped =
        callPackage ./pkgs/development/libraries/aspell-wrapped {};

      # VSC stuff
      # =========
      my-vscode = callPackage ./pkgs/applications/editors/my-vscode {
        terraform = final.terraform.withPlugins (p: [p.aws]);
      };

      # Haskell stuff
      # =============
      haskell-mk = callPackage ./../src/haskell/haskell-mk/nix {};
      haskell-lib-mk = callPackage ./../src/haskell/haskell-lib-mk/nix {};
      haskell-test-mk = callPackage ./../src/haskell/haskell-test-mk/nix {};

      adventlib = final.haskellPackages.adventlib;
      adventlib-old-1 = final.haskellPackages.adventlib-old-1;
      boardgamer = final.haskellPackages.boardgamer;
      boollib = final.haskellPackages.boollib;
      clean-clocks = final.haskellPackages.clean-clocks;
      hashcode-photoalbum = final.haskellPackages.hashcode-photoalbum;
      low-battery = final.haskellPackages.low-battery;
      mk-conf-file = final.haskellPackages.mk-conf-file;
      monad-emit = final.haskellPackages.monad-emit;
      name-generator = final.haskellPackages.name-generator;
      onirim-helper = final.haskellPackages.onirim-helper;
      parselib = final.haskellPackages.parselib;
      perlude = final.haskellPackages.perlude;
      searchlib = final.haskellPackages.searchlib;

      # Shell-scripts
      # =============
      assorted-scripts = callPackage ./../src/shell/assorted-scripts/nix {
        inherit (final.xorg) xbacklight xrandr xset;
      };

      sh-lib = callPackage ./../src/shell/sh-lib/nix {};

      commit-hook-ticket-prefix =
        callPackage ./../src/shell/commit-hook-ticket-prefix/nix {};

      sandbox = callPackage ./../src/shell/sandbox/nix {};

      # C++ stuff
      # =========
      reservoir = callPackage ./../src/c++/reservoir/nix {};

      monte-carlo = callPackage ./../src/c++/monte-carlo/nix {};
      algos-n-fun = callPackage ./../src/c++/algos-n-fun/nix {};
      finndb = callPackage ./../src/c++/finndb/nix {};
      graphlib = callPackage ./../src/c++/graphlib/nix {};
      rndlib = callPackage ./../src/c++/rndlib/nix {};
      asyncq = callPackage ./../src/c++/asyncq/nix {};

      udp-cat = final.callPackage ./pkgs/applications/networking/tools/udp-cat {};

      # Contests, puzzles, etc
      # ======================
      adventofcode-2019 = final.haskellPackages.adventofcode-2019;
      adventofcode-2020 = final.haskellPackages.adventofcode-2020;
      adventofcode-2021 = final.haskellPackages.adventofcode-2021;
    };
  };
in
  pkgs // pkgs.derivations-sam
