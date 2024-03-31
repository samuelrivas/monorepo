# We load this function in flake.nix to overlay all our packages over the
# nixpkgs collection.
#
# Note that this is incomplete, several packages are only available via the
# legacy nix/default.nix. We'll keep migrating those to here and eventually
# deprecate nix/default.nix.
final: prev: let
  system-lib = import ./system-lib.nix {
    # TODO make packages-xxx actually refer to the specific set that they should refer to
    packages-nixpkgs = final.pkgs;
    packages-sam = final.pkgs;
  };
  packages = import ./packages.nix {
    lib = prev.lib;
    # TODO: The flake should pass this already overlayed
    system-lib = {sam = system-lib;};
    nixpkgs = prev;
    # TODO: do get the aactual vscode-marketplace set here instead of the whole
    # set of overlaid packages
    vscode-extensions = final.pkgs;
  };
in
  # pkgs // pkgs.derivations-sam
  packages // {derivations-sam = packages;}
