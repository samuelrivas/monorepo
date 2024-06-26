# A nix tree of everything

[![Build Status](https://app.travis-ci.com/samuelrivas/monorepo.svg?branch=main)](https://app.travis-ci.com/samuelrivas/monorepo)

## Usage

From the top level, you can build everything by running

    nix build .#all-pkgs-sam

provided you have nix installed. It will take a while the first time you run it,
but nix will only rebuild what's necessary afterwards.

You can see all available packages by running

    nix flake show

And build any of them by running

    nix .#<package>

After that, `result` contains a link to the built package

You can obviously install any of those packages in your environment by running

    nix profile install .#<package>

By convention, our derivations may include a `dev-shell` value with a derivation
that can be used to create a nix shell. That is, for a package `foo`, `nix-sell
foo.sandbox` drops you in de dev enviromnet for `foo`. `X.dev-shell` value is
typically the same as `X`, with some `buildInputs` added.

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
