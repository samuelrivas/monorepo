{
  lib-nixpkgs, # lib from nixpkgs
  lib-sam, # lib from this monorepo
  lib-system, # system dependent libs
  nixpkgs, # official nixpkgs set
}: let
  builders = lib-system.sam.builders;
  derivation-helpers = lib-sam.derivation-helpers;
  callPackage =
    lib-nixpkgs.callPackageWith
    (nixpkgs // packages // builders // derivation-helpers);
  haskellPackages = nixpkgs.haskellPackages.override {
    overrides = h-pkgs: h-prev: let
      h-package = h-pkgs.callPackage;
    in {
      inherit (lib-system.sam.haskell) haskell-pkg haskell-lib-pkg;
      inherit (packages) kattis-cli;

      adventlib = h-package ./../src/haskell/adventlib/nix {};
      adventlib-old-1 = h-package ./../src/haskell/adventlib-old-1/nix {};
      adventofcode-2019 = h-package ./../src/puzzles/adventofcode/2019/nix {};
      adventofcode-2020 = h-package ./../src/puzzles/adventofcode/2020/nix {};
      adventofcode-2021 = h-package ./../src/puzzles/adventofcode/2021/nix {};
      adventofcode-2023 = h-package ./../src/puzzles/adventofcode/2023/nix {};
      boardgamer = h-package ./../src/haskell/boardgamer/nix {};
      boollib = h-package ./../src/haskell/boollib/nix {};
      clean-clocks = h-package ./../src/haskell/clean-clocks/nix {};
      hashcode-photoalbum = h-package ./../src/haskell/hashcode-photoalbum/nix {};
      kattis = h-package ./../src/puzzles/kattis/nix {};
      low-battery = h-package ./../src/haskell/low-battery/nix {
        # there is a libnotify in haskellPackages that aliases this one
        inherit (nixpkgs) libnotify;
      };
      mk-conf-file = h-package ./../src/haskell/mk-conf-file/nix {};
      monad-emit = h-package ./../src/haskell/monad-emit/nix {};
      name-generator = h-package ./../src/haskell/name-generator/nix {};
      onirim-helper = h-package ./../src/haskell/onirim-helper/nix {};
      parselib = h-package ./../src/haskell/parselib/nix {};
      perlude = h-package ./../src/haskell/perlude/nix {};
      searchlib = h-package ./../src/haskell/searchlib/nix {};
    };
  };

  packages = {
    # Emacs stuff
    # ===========
    emacs-config = callPackage ./../src/elisp/emacs-config/nix {};

    # An emacs wrapper with the needed packages accessible
    my-emacs = let
      emacs-package =
        if nixpkgs.stdenv.isDarwin
        then nixpkgs.emacs-macport
        else nixpkgs.emacs;
    in
      callPackage ./pkgs/applications/editors/my-emacs {
        emacs = emacs-package;
        inherit (nixpkgs) stdenv;
        inherit (nixpkgs.python3Packages) jedi-language-server;
      };

    # Everything in this set must be a derivation. Store files are not
    # derivations, so we need to wrap the configuration file into a derivation
    # to output it here
    my-emacs-config-file =
      nixpkgs.runCommandLocal
      "config.el"
      {}
      "ln -s ${packages.emacs-config.passthru.config-file} $out";

    # Haskell stuff
    # =============
    haskell-mk = callPackage ./../src/haskell/haskell-mk/nix {};
    haskell-lib-mk = callPackage ./../src/haskell/haskell-lib-mk/nix {};
    haskell-test-mk = callPackage ./../src/haskell/haskell-test-mk/nix {};

    adventlib = haskellPackages.adventlib;
    adventlib-old-1 = haskellPackages.adventlib-old-1;
    boardgamer = haskellPackages.boardgamer;
    boollib = haskellPackages.boollib;
    clean-clocks = haskellPackages.clean-clocks;
    hashcode-photoalbum = haskellPackages.hashcode-photoalbum;
    low-battery = haskellPackages.low-battery;
    mk-conf-file = haskellPackages.mk-conf-file;
    monad-emit = haskellPackages.monad-emit;
    name-generator = haskellPackages.name-generator;
    onirim-helper = haskellPackages.onirim-helper;
    parselib = haskellPackages.parselib;
    perlude = haskellPackages.perlude;
    searchlib = haskellPackages.searchlib;

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

    # LaTeX stuff
    # ===========
    latex-base = callPackage ./../src/docs/latex-base/nix {
      inherit (nixpkgs.texlivePackages) digestif;
    };

    # Go stuff
    # ========
    passman-go = callPackage ./../src/go/passman/nix {
      inherit (nixpkgs.ocamlPackages) sexp;
      inherit (lib-nixpkgs) makeBinPath;
    };

    # Tools wrapped from external repos
    # =================================
    kattis-cli = callPackage ./pkgs/applications/networking/tools/kattis/nix {};
    udp-cat = callPackage ./pkgs/applications/networking/tools/udp-cat {};

    # Contests, puzzles, etc
    # ======================
    adventofcode-2019 = haskellPackages.adventofcode-2019;
    adventofcode-2020 = haskellPackages.adventofcode-2020;
    adventofcode-2021 = haskellPackages.adventofcode-2021;
    adventofcode-2023 = haskellPackages.adventofcode-2023;
    kattis = haskellPackages.kattis;

    # External tools that are broken upstream
    cpplint = callPackage ./pkgs/development/tools/cpplint {};
  };
in
  packages
