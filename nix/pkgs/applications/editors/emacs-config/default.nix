{ color-theme-solarized,
  erlang-mode,
  extra-config,
  full-user-name,
  haskellMode,
  merlin,
  modes,
  nix,
  ocpIndent,
  scalaMode2,
  stdenv,
  tuaregMode,
  user,
  utop,
  writeTextFile
}:
let
  mode-config = mode: text:
    if builtins.elem mode modes
    then text
    else "";
in
stdenv.mkDerivation rec {

  name = "emacs-config";

  static-config  = ./static.el;
  static-haskell = ./static-haskell.el;
  static-erlang  = ./static-erlang.el;
  static-scala   = ./static-scala.el;
  static-ocaml   = ./static-ocaml.el;

  erlang-config = mode-config "erlang" ''
    (add-to-list 'load-path "${erlang-mode}/share/emacs/site-lisp")
    (require 'erlang-start)
    (load "${static-erlang}")
  '';

  haskell-config = mode-config "haskell" ''
    (add-to-list 'load-path "${haskellMode}/share/emacs/site-lisp")
    (require 'haskell-mode)
    (load "${static-haskell}")
  '';

  ocaml-config = mode-config "ocaml" ''
    (add-to-list 'load-path "${merlin}/share/emacs/site-lisp")
    (add-to-list 'load-path "${ocpIndent}/share/emacs/site-lisp")
    (load "${tuaregMode}/share/emacs/site-lisp/tuareg-site-file")
    (require 'merlin)
    (require 'ocp-indent)
    (setq merlin-command "${merlin}/bin/ocamlmerlin")
    (setq ocp-indent-path "${ocpIndent}/bin/ocp-indent")
    (setq utop-command "${utop}/bin/utop -emacs")
    (load "${static-ocaml}")
  '';

  nix-config = mode-config "nix" ''
    (add-to-list 'load-path "${nix}/share/emacs/site-lisp")
    (require 'nix-mode)
  '';

  scala-config = mode-config "scala" ''
    (add-to-list 'load-path "${scalaMode2}/share/emacs/site-lisp")
    (require 'scala-mode2)
    (load "${static-scala}")
  '';

  base-config = writeTextFile {
    name = "emacs-config-base.el";
    text = ''
      ;; Color Theme Path
      (add-to-list 'custom-theme-load-path "${color-theme-solarized}/share/emacs/site-lisp")

      ;; User specific info
      (setq user-full-name "${full-user-name}")

      ;; Modes
      ${erlang-config}
      ${haskell-config}
      ${ocaml-config}
      ${nix-config}
      ${scala-config}

      ;; Nix profile (for modes installed in the environment)
      (add-to-list 'load-path "/home/${user}/.nix-profile/share/emacs/site-lisp")

      ;; Load the static configuration
      (load "${static-config}")

      ;; Extra config added by the extra-config option of the emacs-config module
      ${extra-config}
    '';
  };

  buildCommand = ''
    DIR=$out/etc/${user}-config
    mkdir $DIR -p
    ln -s ${base-config} $DIR/emacs.el
  '';
}
