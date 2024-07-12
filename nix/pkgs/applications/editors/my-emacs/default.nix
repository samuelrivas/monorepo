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
  copilot,
  eglot,
  emacs-config,
  emacsWithPackages,
  flycheck-haskell,

  # FIXME: go is not properly added as a dependency, it ends up as a broken link
  # in the depenedencies derivation, this nix fixing in emacsWithPackages. At
  # the moment, you need to install go in your profile, with tout that eglot
  # cannot start
  go,
  go-mode,
  jedi-language-server,
  git,
  gopls,
  groovy-mode,
  helm,
  helm-ls-git,
  helm-org,
  htmlize,
  markdown-mode,
  nil,
  nix-mode,
  projectile,
  silver-searcher,
  stylish-haskell,
  terraform-mode,
  terraform-ls,
  yaml-mode,
  yasnippet,
}:
emacsWithPackages [
  aspell-wrapped
  colorThemeSolarized
  company
  copilot
  eglot
  emacs-config
  flycheck-haskell
  go
  go-mode
  jedi-language-server
  git
  gopls
  groovy-mode
  helm
  helm-ls-git
  helm-org
  htmlize
  markdown-mode
  nil
  nix-mode
  projectile
  silver-searcher
  stylish-haskell
  terraform-ls
  terraform-mode
  yaml-mode
  yasnippet
]
