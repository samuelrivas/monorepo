{
  emacs,
  haskell-mk,
  haskell-lib-mk,
  pkgs,
}:
rec {
  # A utility to instantiate a capable emacs in a haskell sandbox
  emacs-for-haskell = haskell-env: emacs.override { ghc = haskell-env; };

  # Create a haskell package with haskell-mk included and enough meta to create
  # shell environments
  #
  # FIXME: There are "official" ways of doing this in nixpkgs now, may be a good
  # idea to rework this to be more standard
  #
  # FIXME: There is a fair amount of duplication between haskell-pkg and
  # haskell-lib-pkg
  haskell-pkg =
    { haskellPackages,
      name,
      src,
      haskell-libs,
      extra-build-inputs ? [],
      extra-drv ? { },
    }:
    let
      ghc = haskellPackages.ghcWithPackages (_: haskell-libs);
      drv-args = {

        inherit name src;

        buildInputs = [
          ghc
          haskell-mk
        ] ++ extra-build-inputs;

        installPhase = ''
            mkdir -p $out/bin
            cp ../build/bin/* $out/bin
          '';

        meta = {
          inherit haskellPackages ghc;
        };
      } // extra-drv;
      drv = pkgs.stdenv.mkDerivation drv-args;
    in
      drv // {
        sandbox = haskell-shell drv;
      };

  haskell-lib-pkg =
    { haskellPackages,
      name,
      src,
      haskell-libs,
      extra-build-inputs ? [],
      extra-drv ? { },
    }:
    let
      ghc = haskellPackages.ghcWithPackages (_: haskell-libs);
      drv-args = {

        inherit name src;

        buildInputs = [
          ghc
          haskell-lib-mk
        ] ++ extra-build-inputs;

        propagatedBuildInputs = haskell-libs;
        installPhase = ''
          make PREFIX="$out" install
        '';

        # Silently required by ghcWithPackages, for some reason
        isHaskellLibrary = true;

        meta = {
          inherit haskellPackages ghc;
        };
      } // extra-drv;
      drv = pkgs.stdenv.mkDerivation drv-args;
    in
      drv // {
        sandbox = haskell-shell drv;
      };

  # Use the package's meta to create a derivation that can be used to start a
  # nix shell with a configured emacs and hoogle
  #
  # For this to work, haskell-drv need to expose ghc and haskellPackages in the
  # `meta` argument of the derivation. HAskell packages created with
  # `haskell-pkg` create a suitable `meta` by default.
  haskell-shell = haskell-drv:
    haskell-drv.overrideAttrs (attrs:

      { buildInputs = attrs.buildInputs ++ [
          (emacs-for-haskell haskell-drv.meta.ghc)
          haskell-drv.meta.haskellPackages.hoogle
          haskell-drv.meta.haskellPackages.haskell-language-server
        ];
      });

  # Add my haskell libraries to a given set of haskell packages
  #
  # packages-to-add is a lambda taking a haskellPackages and returning a set
  # with new packages to add.
  mk-haskell-packages =
    haskellPackages: packages-to-add:
    (haskellPackages.override {
      overrides = self: super: {
        inherit haskell-pkg haskell-lib-pkg haskell-lib-mk;
      } // (packages-to-add self);
    });
}
