{
  emacs,
  empty-builder,
  haskell,
  haskellPackages,
  sandbox,
  stdenv,
}:
let
  # Try to unbreak broken stuff
  hackedHaskellPackages = haskellPackages.override {
    overrides = haskellPackagesNew: haskellPackagesOld: rec {
      threadscope = haskell.lib.doJailbreak haskellPackagesOld.threadscope;
      # rank1dynamic = haskell.lib.dontCheck haskellPackagesOld.rank1dynamic;
      # distributed-static = haskell.lib.doJailbreak haskellPackagesOld.distributed-static;
      # distributed-process = haskell.lib.doJailbreak haskellPackagesOld.distributed-process;
    };
  };
  wanted-packages = with hackedHaskellPackages; [
    parallel
    abstract-par
    accelerate

    ## These are broken right now, and I don't need them, so just wait for a while...
    ## ---
    # distributed-process
    # distributed-process-simplelocalnet
    # http-conduit
    hoogle
    monad-par
    normaldistribution
    repa
    utf8-string
    xml
    threadscope
  ];
  haskell-packages-selector = pkgs: wanted-packages;
  ghc = hackedHaskellPackages.ghcWithPackages haskell-packages-selector;
in
stdenv.mkDerivation rec {

  name = "concurrent-haskell";
  src = ./../src;

  buildInputs = [
    ghc
    hackedHaskellPackages.hoogle
    hackedHaskellPackages.hlint
  ] ++ (if sandbox then [(emacs.override { inherit ghc; })] else []);

  installPhase = ''
    mkdir -p $out/bin
    cp ../build/bin/* $out/bin
  '';
}
