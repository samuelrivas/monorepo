{
  copilot,
  emacs-mcx,
  vscode-with-extensions,
  vscodium,
}:
vscode-with-extensions.override {
  vscode = vscodium;
  vscodeExtensions = [
    copilot
    emacs-mcx
  ];
}
