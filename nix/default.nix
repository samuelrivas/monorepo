# This is the root package set, it contains all packages from `nixpkgs`, and a
# few added by us. All packages we add are also part of the pkgs-sam subset. The
# rest of nix expressions are structured following these guidelines:
#
#  * For the sources contained in this monorepo, the nix expressions to build
#    them are colocated with the source and linked from here
#  * For the packages that use external sources (usually packages that are
#    patches to the official nix tree) we use nixpkgs-like struture inside pkgs
#
# Whenever we break compatibility without fixing all the dependencies we create
# an old version of the derivation that changed. For a derivation `foo`, we name
# these old versions as `foo-old-1`, `foo-old-2`, ... where the lowest the
# number the oldest the copy. Those derivations that are not updated to use the
# upstream version of the changed derivation stay dependant on the old version
# that they are compatible with.
{ system ? builtins.currentSystem }:

let
  nixpkgs-stable = import ./nixpkgs.nix { inherit system; };
  nixpkgs-upstream = import ./nixpkgs-upstream.nix { inherit system; };
  nixpkgs-patched = import ./nixpkgs-patched.nix { inherit system; };
in
import ./pkgs-sam.nix {
  inherit
    nixpkgs-stable
    nixpkgs-upstream
    nixpkgs-patched;
}
