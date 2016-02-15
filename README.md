# A nix tree of everything

[![Build Status](https://travis-ci.org/samuelrivas/monorepo.svg?branch=master)](https://travis-ci.org/samuelrivas/monorepo)

## Usage

From the top level, you can build everything by running

    nix-build nix

provided you have nix installed. It will take a while the first time you run it,
but nix will only rebuild what's necessary afterwards.

You can see all available packages by running

    nix-env -qa -f nix

And build any of them by running

    nix-build -A <package> nix

After that, result contains a link to the built package

You can obviously install any of those packages in your environemnt by running

    nix-env -i <package> -f nix

## Configuration

Some packages can be configured, you can personalize some options by writing a
nix expression in

    $HOME/.local-nix-config/configuration.nix

For example:

    {
      emacs-config = {
        user = "samuel";
        full-user-name = "Samuel Rivas";
        blacklisted-modes = [ "ocaml" ];
      };
    }

All possible configuration options are defined in nix/modules

## Conventions for contributions

Avoid generating files along with the sources. Since I use nix to build stuff,
I'd like to prevent it rebuilding things that have been compiled and stored in
the store. However most standard project structures create artifacts in the same
tree directory where the sources are. By default, Nix scans the whole source
directory and recreates a different package if anything has changed there. Thus
I would prefer layouts like this (with the nix expressions in `nix/` pointing to
the first `src` level:

    project
      |
      +-- nix/
      |
      +-- src/
      |    |
      |    +-- src/
      |    |
      |    +-- doc/
      |    |
      |    +-- Makefile
      |    |
      |    +-- ...
      |
      +-- build/

The double src directory is just to preserve the most common structure of
sources, which usually includes a source directory, while isolating the
compilation producs from the `immutable` source from nix's point of view. The
Makefile should strive to just generate things in the build directory.

Additionally, I'd like editors not to generate backups along with the sources
either, but that is up to you, since they must not be checked in the repository.
