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
  standard-extensions = with vscode-marketplace; [
    charliermarsh.ruff
    github.copilot
    hashicorp.terraform
    haskell.haskell
    james-yu.latex-workshop
    jmaxwilson.vscode-povray
    jnoortheen.nix-ide
    justusadam.language-haskell
    ms-python.python
    nicolasvuillamy.vscode-groovy-lint
    stkb.rewrap
    streetsidesoftware.code-spell-checker
    tamasfe.even-better-toml
    tsandall.opa
    tuttieee.emacs-mcx
    yzhang.markdown-all-in-one
  ];
  patched-extensions =
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
    vscodeExtensions = standard-extensions ++ patched-extensions;
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
