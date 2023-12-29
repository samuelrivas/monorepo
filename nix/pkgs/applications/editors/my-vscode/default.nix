{
  alejandra,
  haskell-language-server,
  lib,
  nil,
  haskellPackages,
  vscode-marketplace,
  vscode-with-extensions,
  vscodium,
  writeShellScriptBin,
}: let
  executable = vscode-with-extensions.override {
    vscode = vscodium;
    vscodeExtensions = with vscode-marketplace; [
      github.copilot
      haskell.haskell
      jnoortheen.nix-ide
      justusadam.language-haskell
      tuttieee.emacs-mcx
      yzhang.markdown-all-in-one
    ];
  };
  path = lib.makeBinPath [
    alejandra
    haskell-language-server
    haskellPackages.ghc
    haskellPackages.ormolu.bin
    nil
  ];
in
  writeShellScriptBin "codium"
  ''
    export PATH=${path}:$PATH
    exec ${executable}/bin/codium "$@"
  ''
