{
  vscode-marketplace,
  vscode-with-extensions,
  vscodium,
}:
vscode-with-extensions.override {
  vscode = vscodium;
  vscodeExtensions = with vscode-marketplace; [
    github.copilot
    jnoortheen.nix-ide
    tuttieee.emacs-mcx
  ];
}
