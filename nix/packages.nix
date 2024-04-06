{
  legacy-lib,
  lib, # system indepentent libs, including nixpkgs lib plus our overlaid sam lib
  system-lib, # system dependent libs
  nixpkgs, # official nixpkgs set
  vscode-extensions,
}: let
  builders = system-lib.sam.builders;
  # TODO: remove this use of legacy, we are not using it for anything necessary
  derivation-helpers = legacy-lib.derivation-helpers;
  libs-sam = {inherit builders derivation-helpers;};
  callPackage =
    lib.callPackageWith
    (nixpkgs // pkgs.derivations-sam // builders // derivation-helpers);
  pkgs = {
    # We keep our libraries in a separate attrset and pass them with
    # callPackageWith instead of polluting the top level namespace with them. We
    # do add all our pakcages to the top level namespace though
    inherit libs-sam system-lib;

    sam.system-lib = system-lib;
    haskell-lib = system-lib.haskell;

    haskellPackages = nixpkgs.haskellPackages.override {
      overrides = h-pkgs: h-prev: let
        h-package = h-pkgs.callPackage;
      in {
        inherit (system-lib.sam.haskell) haskell-pkg haskell-lib-pkg;

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
          (nixpkgs.emacsPackages)
          dash
          editorconfig
          s
          trivialBuild
          ;
      };

      # An emacs wrapper with the needed packages accessible
      my-emacs = callPackage ./pkgs/applications/editors/my-emacs {
        inherit
          (nixpkgs.emacsPackages)
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
        inherit (nixpkgs.python3Packages) jedi-language-server;
      };

      # aspell needs to be configured to find the dictionaries
      aspell-wrapped =
        callPackage ./pkgs/development/libraries/aspell-wrapped {};

      # VSC stuff
      # =========
      my-vscode = callPackage ./pkgs/applications/editors/my-vscode {
        # TODO: pass marketplace directly instead of the whole extensions package
        inherit (vscode-extensions) vscode-marketplace;
        terraform = nixpkgs.terraform.withPlugins (p: [p.aws]);
      };

      # Haskell stuff
      # =============
      haskell-mk = callPackage ./../src/haskell/haskell-mk/nix {};
      haskell-lib-mk = callPackage ./../src/haskell/haskell-lib-mk/nix {};
      haskell-test-mk = callPackage ./../src/haskell/haskell-test-mk/nix {};

      adventlib = pkgs.haskellPackages.adventlib;
      adventlib-old-1 = pkgs.haskellPackages.adventlib-old-1;
      boardgamer = pkgs.haskellPackages.boardgamer;
      boollib = pkgs.haskellPackages.boollib;
      clean-clocks = pkgs.haskellPackages.clean-clocks;
      hashcode-photoalbum = pkgs.haskellPackages.hashcode-photoalbum;
      low-battery = pkgs.haskellPackages.low-battery;
      mk-conf-file = pkgs.haskellPackages.mk-conf-file;
      monad-emit = pkgs.haskellPackages.monad-emit;
      name-generator = pkgs.haskellPackages.name-generator;
      onirim-helper = pkgs.haskellPackages.onirim-helper;
      parselib = pkgs.haskellPackages.parselib;
      perlude = pkgs.haskellPackages.perlude;
      searchlib = pkgs.haskellPackages.searchlib;

      # Shell-scripts
      # =============
      assorted-scripts = callPackage ./../src/shell/assorted-scripts/nix {
        inherit (nixpkgs.xorg) xbacklight xrandr xset;
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

      udp-cat = callPackage ./pkgs/applications/networking/tools/udp-cat {};

      # Contests, puzzles, etc
      # ======================
      adventofcode-2019 = pkgs.haskellPackages.adventofcode-2019;
      adventofcode-2020 = pkgs.haskellPackages.adventofcode-2020;
      adventofcode-2021 = pkgs.haskellPackages.adventofcode-2021;
    };
  };
in
  pkgs.derivations-sam
