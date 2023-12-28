{
  alejandra,
  lib,
  nil,
  vscode-marketplace,
  vscode-with-extensions,
  vscodium,
  writeShellScriptBin,
}: let
  executable = vscode-with-extensions.override {
    vscode = vscodium;
    vscodeExtensions = with vscode-marketplace; [
      github.copilot
      jnoortheen.nix-ide
      tuttieee.emacs-mcx
      yzhang.markdown-all-in-one
    ];
  };
in
  writeShellScriptBin "codium"
  ''
    export PATH=${lib.makeBinPath [alejandra nil]}:$PATH
    exec ${executable}/bin/codium "$@"
  ''
