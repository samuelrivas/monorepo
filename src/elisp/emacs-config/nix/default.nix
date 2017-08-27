{
  blacklisted-modes,
  emacs,
  extra-config,
  full-user-name,
  org-agenda-file,
  org-interview-file,
  org-meeting-file,
  org-template-dir,
  org-todo-file,
  stdenv,
  writeTextFile
}:

stdenv.mkDerivation rec {

  name = "emacs-config-0.0.0";
  src = ./../src;

  # This loads before anything else
  custom-variables = writeTextFile {
    name = "custom-variables.el";
    text = ''
      ;; User specific info
      (setq user-full-name "${full-user-name}")

      ;; Org mode configuration
      (defvar sams-org-config)
      (setq sams-org-config '(
        :agenda-file "${builtins.toString org-agenda-file}"
        :template-dir "${builtins.toString org-template-dir}"
        :todo-file "${builtins.toString org-todo-file}"
        :meeting-file "${builtins.toString org-meeting-file}"
        :interview-file "${builtins.toString org-interview-file}"))
    '';
  };

  # This loads after the emacs-config.el file
  emacs-custom = writeTextFile {
    name = "emacs-custom.el";
    text = ''
      ;; Extra config added by the extra-config option of the emacs-config module
      ${extra-config}
    '';
  };

  # Loading this file activates all the configuration
  init-file = ./load-my-emacs-config.el;

  buildInputs = [
    emacs
  ];

  # TODO: this adds undesirable coupling with the sources (the makefile will
  # compile these files), there must be a better way
  preBuild = ''
    ln -s ${custom-variables} custom-variables.el
    ln -s ${emacs-custom} emacs-custom.el
    ln -s ${init-file} load-my-emacs-config.el
  '';

  installPhase = ''
    DIR=$out/share/emacs
    mkdir $DIR -p
    cp -r ../build "$DIR/site-lisp"
  '';
}
