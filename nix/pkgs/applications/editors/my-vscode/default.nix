{
  alejandra,
  haskell-language-server,
  lib,
  nil,
  haskellPackages,
  python3,
  terraform,
  vscode-utils,
  vscode-marketplace,
  vscode-with-extensions,
  vscodium,
  writeShellScriptBin,
}: let
  build-extension = ext:
    vscode-utils.buildVscodeMarketplaceExtension {
      mktplcRef = ext;
    };
  standard-exceptions = with vscode-marketplace; [
    github.copilot
    hashicorp.terraform
    haskell.haskell
    jnoortheen.nix-ide
    justusadam.language-haskell
    ms-python.python
    stkb.rewrap
    tamasfe.even-better-toml
    tuttieee.emacs-mcx
    yzhang.markdown-all-in-one
  ];
  patched-exceptions =
    builtins.map build-extension
    [
      {
        publisher = "ms-python";
        name = "vscode-pylance";
        version = "2023.6.40";
        sha256 = "sha256-J5nRoQjUVKaMqs0QJFY0vzutjWZ9dH6O7FXI+ZZIaBQ=";
      }
    ];
  executable = vscode-with-extensions.override {
    vscode = vscodium;
    vscodeExtensions = standard-exceptions ++ patched-exceptions;
  };
  path = lib.makeBinPath [
    alejandra
    haskell-language-server
    haskellPackages.ghc
    haskellPackages.ormolu.bin
    python3
    terraform
    nil
  ];
in
  writeShellScriptBin "codium"
  ''
    export PATH=${path}:$PATH
    exec ${executable}/bin/codium "$@"
  ''
