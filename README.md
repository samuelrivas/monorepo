# A nix tree of everything

[![Build Status](https://travis-ci.org/samuelrivas/monorepo.svg?branch=main)](https://travis-ci.org/samuelrivas/monorepo)

## Usage

From the top level, you can build everything by running

    nix build -f nix pkgs-sam

provided you have nix installed. It will take a while the first time you run it,
but nix will only rebuild what's necessary afterwards.

You can see all available packages by running

    nix-env -qaPA pkgs-sam -f nix

And build any of them by running

    nix-build -f nix pkgs-sam.<package>

After that, `result` contains a link to the built package

You can obviously install any of those packages in your environemnt by running

    nix-env -iA pkgs-sam.<package> -f nix

By convention, my derivations may include a `sandbox` value with a derivation
that can be used to create a nix shell. That is, for a package `foo`, `nix-sell
foo.sandbox` drops you in de dev enviromnet for `foo`. `X.sandbox` value is
typically the same as `X`, with some `buildInputs` added.

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

Avoid generating files along with the sources. nix will rebuild things that have
been compiled and stored in the store if anything changes in the source
directory. Unfortunately, most standard project structures create artifacts in
the same tree directory where the sources are, so expect build scripts and
makefiles to be a bit "special" here. Prefer layouts like this (with the nix
expressions in `nix/` pointing to the first `src` level):

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
sources, which usually includes a `src` directory, while isolating the
compilation producs from the immutable source. The
`Makefile` should only generate things in the build directory.
