# We load this function in flake.nix to overlay all our packages over the
# nixpkgs collection.
#
# Note that this is incomplete, several packages are only available via the
# legacy nix/default.nix. We'll keep migrating those to here and eventually
# deprecate nix/default.nix.
final: prev: let
  system-lib = import ./system-lib.nix {inherit (final) pkgs;};
  packages = import ./packages.nix {
    lib = prev.lib;
    # TODO: The flake should pass this already overlayed
    system-lib = {sam = system-lib;};
    nixpkgs = prev;
    # TODO: do get the aactual vscode-marketplace set here instead of the whole
    # set of overlaid packages
    vscode-extensions = final.pkgs;
  };
  pkgs = {
    derivations-sam = {
      inherit
        (packages)
        assorted-scripts
        sh-lib
        sandbox
        reservoir
        monte-carlo
        algos-n-fun
        finndb
        graphlib
        rndlib
        asyncq
        udp-cat
        emacs-config
        copilot
        my-emacs
        aspell-wrapped
        haskell-mk
        haskell-lib-mk
        haskell-test-mk
        commit-hook-ticket-prefix
        adventlib
        adventlib-old-1
        boardgamer
        boollib
        clean-clocks
        hashcode-photoalbum
        low-battery
        mk-conf-file
        monad-emit
        name-generator
        onirim-helper
        parselib
        perlude
        searchlib
        adventofcode-2019
        adventofcode-2020
        adventofcode-2021
        my-vscode
        ;
    };
  };
in
  pkgs // pkgs.derivations-sam
