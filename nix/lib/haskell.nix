{
  pkgs,
}: {
  # Create a haskell package with haskell-mk included and enough meta to create
  # shell environments
  #
  # We export Werror to fail the build by default (assuming that the package
  # Makefile does not do GHC-FLAGS := ..., but GHC-FLAGS += ...), but you can
  # unset that when running in a sandbox for quick iterations.
  #
  # FIXME: There is a fair amount of duplication between haskell-pkg and
  # haskell-lib-pkg
  haskell-pkg =
    { name,
      src,
      haskell-libs,
      extra-build-inputs ? [],
      extra-drv ? { },
    }:
    let
      ghc = pkgs.haskellPackages.ghcWithPackages (_: haskell-libs);
      drv-args = {

        inherit name src;
        GHC-FLAGS = "-Werror";
        buildInputs = [
          ghc
          pkgs.haskell-mk
          pkgs.haskell-test-mk
        ] ++ extra-build-inputs;

        nativeBuildInputs = [ ];

        installPhase = ''
            mkdir -p $out/bin
            cp ../build/bin/* $out/bin
          '';

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
    { extra-build-inputs ? [],
      extra-native-build-inputs ? [],
      extra-drv ? { },
      haskell-libs,
      name,
      src,
    }:
    let
      ghc = pkgs.haskellPackages.ghcWithPackages (_: haskell-libs);
      drv-args = {

        inherit name src;
        GHC-FLAGS = "-Werror";
        buildInputs = [
        ] ++ extra-build-inputs;

        nativeBuildInputs = [
          ghc
          pkgs.haskell-lib-mk
          pkgs.haskell-test-mk
        ] ++ extra-native-build-inputs;

        propagatedBuildInputs = haskell-libs;
        installPhase = ''
          make PREFIX="$out" install
        '';

        # Silently required by ghcWithPackages, for some reason
        isHaskellLibrary = true;
      } // extra-drv;
      drv = pkgs.stdenv.mkDerivation drv-args;
      dev-shell = drv.overrideAttrs (final: previous: {

        # Emacs uses fontconfig, which needs a writable cache directory
        XDG_CACHE_HOME = "/tmp/cache";
        nativeBuildInputs =
          (builtins.filter (x: x != ghc) previous.nativeBuildInputs)
          ++ [ pkgs.haskell-language-server
               pkgs.my-emacs
               pkgs.git
               pkgs.glibcLocales
               (pkgs.haskellPackages.ghcWithHoogle (_: haskell-libs))
             ];
      });
    in
      drv // { inherit dev-shell; };
}
