# My Emacs Config

This repo contains all the emacs configuration I can store cleanly here. That
is, it doesn't depend on anything too specific, so it can be used by anyone (if
you happened to have exactly the same emacs-taste as me).

For cases where we need some customisation, the nix package `emacs-config`
creates a file you can link as your init file directly or load from your init
file. The nix package can be customised locally.

To build this repo, simply run `make -C src`
