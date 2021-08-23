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
      let
        spinner-version = "1.7.3";
        spinner-file = "spinner-${spinner-version}.el";
        spinner-lzip = builtins.fetchurl {
          url = "https://elpa.gnu.org/packages/${spinner-file}.lz";
          sha256 = "188i2r7ixva78qd99ksyh3jagnijpvzzjvvx37n57x8nkp8jc4i4";
        };
      in {
        # spinner got broken because elpa lzips old versions. See
        # https://discourse.nixos.org/t/how-to-override-an-emacs-package-src-url-to-fix-404/13947/6
        spinner = super.spinner.override {
          elpaBuild = args: super.elpaBuild (args // {
            src = pkgs.runCommandLocal spinner-file {} ''
              ${pkgs.lzip}/bin/lzip -d -c ${spinner-lzip} > $out
            '';
          });
        };
      });

    # An emacs wrapper with the needed packages accessible
    emacs = callPackage ./pkgs/applications/editors/my-emacs
      (with pkgs; {
        inherit (pkgs-sam.emacsPackages)
          dumb-jump
          colorThemeSolarized
          company
          erlangMode
          flycheck-haskell
          groovy-mode
          haskell-mode
          helm
          helm-ls-git
          helm-org
          htmlize
          lsp-haskell
          markdown-mode
          nix-mode
          projectile
          scalaMode2
          terraform-mode
          tuareg
          yaml-mode
          yasnippet;
        inherit (haskellPackages) hlint stylish-haskell;
        inherit (ocamlPackages) merlin ocp-indent utop;
        emacs-config-options = pkgs-sam.local-config.emacs-config;
      });

    # aspell needs to be configured to find the dictionaries
    aspell-wrapped = callPackage ./pkgs/development/libraries/aspell-wrapped { };

    # Haskell stuff
    # =============

    # FIXME: haskell-mk (and maybe emacs) should be passed as arguments to the
    # derivations, currently we are implicitly adding them as dependencies
    # because they are part of haskell-lib. The problem with the current setting
    # is that we cannot change any of them without affceting all our haskell
    # packages.
    haskell-mk = callPackage ./../src/haskell/haskell-mk/nix {  };
    haskell-lib = import ./lib/haskell.nix {
      inherit pkgs;
      inherit (pkgs-sam) emacs haskell-mk;
    };

    profiledHaskellPackages = pkgs.haskellPackages.override {
      overrides = pkgs-sam: super: {
        mkDerivation = args: super.mkDerivation (args // {
          enableLibraryProfiling = true;
        });
      };
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
        adventofcode-2020 = ./../src/puzzles/adventofcode/2019/nix;
        example-lib =  ./../src/haskell/example-lib/nix;
        name-generator =  ./../src/haskell/name-generator/nix;
        boardgamer = ./../src/haskell/boardgamer/nix;
        hashcode-photoalbum =  ./../src/haskell/hashcode-photoalbum/nix;
        onirim-helper = ./../src/haskell/onirim-helper/nix;
        low-battery = ./../src/haskell/low-battery/nix;
      };

    name-generator = pkgs-sam.haskellPackages.name-generator;
    boardgamer = pkgs-sam.haskellPackages.boardgamer;
    hashcode-photoalbum = pkgs-sam.haskellPackages.hashcode-photoalbum;
    onirim-helper = pkgs-sam.haskellPackages.onirim-helper;
    low-battery = pkgs-sam.haskellPackages.low-battery;

    # Shell-scripts
    # =============
    assorted-scripts = callPackage ./../src/shell/assorted-scripts/nix {
      inherit (pkgs.xlibs) xbacklight xrandr xset;
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

    # Contests, puzzles, etc
    # ======================
    adventofcode-2019 = pkgs-sam.haskellPackages.adventofcode-2019;
    adventofcode-2020 = pkgs-sam.haskellPackages.adventofcode-2020;
  };
in
# All official packages plus ours. We also add pkgs-sam as a set with all our
# packages so that we can run nix-build -A pkgs-sam and test this monorepo
pkgs-all // { inherit pkgs-sam; }
