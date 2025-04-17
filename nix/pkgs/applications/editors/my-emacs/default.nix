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
  # the moment, you need to install go in your profile, without that eglot
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
  helm-projectile,
  htmlize,
  markdown-mode,
  nil,
  nix-mode,
  nodejs,
  projectile,
  s,
  silver-searcher,
  stdenv,
  stylish-haskell,
  symlinkJoin,
  terraform-ls,
  terraform-mode,
  texliveMedium,
  writeShellScriptBin,
  yaml-mode,
  yasnippet,
}: let
  # Emacs sometimes uses its own `exec-path` to launc binaries and sometimes it
  # fires up a shell and launches binaries from there. At least latex preview
  # cannot find dvipng
  exec-deps = symlinkJoin {
    name = "my-emacs-exec-deps";
    paths = [
      texliveMedium
    ];
  };
  # If we call the regular `emacs` file on darwin, it won't behave correctly
  # with the window manager. Thus, we substitute `emacs` with a script that runs
  # the installed mac application, which does behave correctly.
  wrapDarwin = drv:
    writeShellScriptBin
    "emacs"
    ''
      export PATH=$PATH:${exec-deps}/bin
      exec "${drv}/Applications/Emacs.app/Contents/MacOS/Emacs" "$@"
    '';
  wrapLinux = drv:
    writeShellScriptBin
    "emacs"
    ''
      export PATH=$PATH:${exec-deps}/bin
      exec "${drv}/bin/emacs" "$@"
    '';
  wrap =
    if stdenv.isDarwin
    then wrapDarwin
    else wrapLinux;
in
  wrap (emacsWithPackages [
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
    helm-projectile
    htmlize
    markdown-mode
    nil
    nix-mode
    nodejs # Needed by copilot
    projectile
    s # s seems to be needed by org mode, but it doesn't depend on it so we need to add it explicitly
    silver-searcher
    stylish-haskell
    terraform-ls
    terraform-mode
    yaml-mode
    yasnippet
  ])
