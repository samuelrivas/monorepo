{
  haskell-mk,
  haskell-lib-mk,
  haskell-test-mk,
  pkgs,
}:
rec {
  # Create a haskell package with haskell-mk included and enough meta to create
  # shell environments
  #
  # We export Werror to fail the build by default (assuming that the package
  # Makefile does not do GHC-FLAGS := ..., but GHC-FLAGS += ...), but you can
  # unset that when running in a sandbox for quick iterations.
  #
  # FIXME: There is a fair amount of duplication between haskell-pkg and
  # haskell-lib-pkg
  #
  # FIXME: ghcWithPackages is probably not necessary
  haskell-pkg =
    { name,
      src,
      ghcWithPackages,
      haskell-libs,
      extra-build-inputs ? [],
      extra-drv ? { },
    }:
    let
      ghc = ghcWithPackages (_: haskell-libs);
      drv-args = {

        inherit name src;
        GHC-FLAGS = "-Werror";
        buildInputs = [
          ghc
          haskell-mk
          haskell-test-mk
        ] ++ extra-build-inputs;

        # We use overrideAttrs to create the shell derivation, so we need to
        # explicitly add all the attrs we want to override.  derivation
        nativeBuildInputs = [ ];

        installPhase = ''
            mkdir -p $out/bin
            cp ../build/bin/* $out/bin
          '';

        meta = {
          inherit ghc;
        };
      } // extra-drv;
      drv = pkgs.stdenv.mkDerivation drv-args;
      self =
        drv // {
          dev-shell = self.overrideAttrs (final: previous: {
            nativeBuildInputs =
              previous.nativeBuildInputs ++ [ pkgs.haskell-language-server ];
          });
        };
      in self;

  haskell-lib-pkg =
    { ghcWithPackages,
      name,
      src,
      haskell-libs,
      extra-build-inputs ? [],
      extra-drv ? { },
    }:
    let
      ghc = ghcWithPackages (_: haskell-libs);
      drv-args = {

        inherit name src;
        GHC-FLAGS = "-Werror";
        buildInputs = [
          ghc
          haskell-lib-mk
          haskell-test-mk
        ] ++ extra-build-inputs;

        # We use overrideAttrs to create the shell derivation, so we need to
        # explicitly add all the attrs we want to override.
        nativeBuildInputs = [ ];

        propagatedBuildInputs = haskell-libs;
        installPhase = ''
          make PREFIX="$out" install
        '';

        # Silently required by ghcWithPackages, for some reason
        isHaskellLibrary = true;

        meta = {
          inherit ghc;
        };
      } // extra-drv;
      drv = pkgs.stdenv.mkDerivation drv-args;
      self =
        drv // {
          dev-shell = self.overrideAttrs (final: previous: {
            nativeBuildInputs =
              previous.nativeBuildInputs ++ [ pkgs.haskell-language-server ];
          });
        };
      in self;
}
