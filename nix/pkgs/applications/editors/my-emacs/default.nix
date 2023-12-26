## Generate a wrapped emacs with all required libraries and executables in the
## path
##
## "all" here means, anything required by the configuration generated by the
## emacs-config derivation in this monorepo
##
## Since some packages carry some heavy dependencies with them, you can
## denylist them if you need to generate an emacs but don't want to spend the
## time on waiting for a plethora of things to download (check emacs-config.nix)
{
  aspell-wrapped,
  colorThemeSolarized,
  company,
  copilot-el,
  eglot,
  emacs-config,
  emacsWithPackages,
  flycheck-haskell,
  git,
  groovy-mode,
  haskell-mode,
  helm,
  helm-ls-git,
  helm-org,
  htmlize,
  markdown-mode,
  nix-mode,
  projectile,
  silver-searcher,
  stylish-haskell,
  terraform-mode,
  yaml-mode,
  yasnippet,
}:
emacsWithPackages [
  aspell-wrapped
  colorThemeSolarized
  company
  copilot-el
  eglot
  emacs-config
  flycheck-haskell
  git
  groovy-mode
  helm
  helm-ls-git
  helm-org
  htmlize
  markdown-mode
  nix-mode
  projectile
  silver-searcher
  stylish-haskell
  terraform-mode
  yaml-mode
  yasnippet
]
