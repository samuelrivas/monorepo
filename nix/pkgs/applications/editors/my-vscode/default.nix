{
  alejandra,
  haskell-language-server,
  lib,
  nil,
  haskellPackages,
  python3,
  vscode-marketplace,
  vscode-with-extensions,
  vscodium,
  writeShellScriptBin,
}: let
  executable = vscode-with-extensions.override {
    vscode = vscodium;
    vscodeExtensions = with vscode-marketplace; [
      github.copilot
      hashicorp.terraform
      haskell.haskell
      jnoortheen.nix-ide
      justusadam.language-haskell
      ms-python.python
      ms-python.vscode-pylance
      tuttieee.emacs-mcx
      yzhang.markdown-all-in-one
    ];
  };
  path = lib.makeBinPath [
    alejandra
    haskell-language-server
    haskellPackages.ghc
    haskellPackages.ormolu.bin
    python3
    nil
  ];
in
  writeShellScriptBin "codium"
  ''
    export PATH=${path}:$PATH
    exec ${executable}/bin/codium "$@"
  ''
