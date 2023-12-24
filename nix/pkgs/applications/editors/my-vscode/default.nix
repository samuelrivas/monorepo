{
  copilot,
  vscode-with-extensions,
  vscodium,
}:
vscode-with-extensions.override {
  vscode = vscodium;
  vscodeExtensions = [
    copilot
  ];
}
