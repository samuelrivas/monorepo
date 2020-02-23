{
  pkgs-sam,
  pkgs,
} :
{
  # A utility to instantiate a capable emacs in a haskell sandbox
  emacs-for-haskell = haskell-env: pkgs-sam.emacs.override { ghc = haskell-env; };

  haskell-pkg =
    { haskellPackages ? pkgs.haskellPackages,
      name,
      sandbox ? false,
      src,
      wanted-packages,
      extra-build-inputs ? [],
    } :
    let
      haskell-packages-selector = _: wanted-packages;
      ghc = haskellPackages.ghcWithPackages haskell-packages-selector;
    in
      pkgs.stdenv.mkDerivation rec {

        inherit name src;

        buildInputs = [
          ghc
          pkgs-sam.haskell-mk
        ]
        ++ extra-build-inputs
        ++ (if sandbox
            then [(pkgs-sam.emacs-for-haskell ghc) haskellPackages.hoogle]
            else []);

        installPhase = ''
            mkdir -p $out/bin
            cp ../build/bin/* $out/bin
          '';
      };

  profiledHaskellPackages = pkgs.haskellPackages.override {
    overrides = _pkgs: super: {
      mkDerivation = args: super.mkDerivation (args // {
        enableLibraryProfiling = true;
      });
    };
  };
}
