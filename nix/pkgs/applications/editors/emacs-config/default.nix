{
  emacs,
  extra-config,
  full-user-name,
  blacklisted-modes,
  stdenv,
  writeTextFile
}:

stdenv.mkDerivation rec {

  name = "emacs-config-0.0.0";
  src = ./../../../../../src/elisp/emacs-config/src;

  emacs-custom = writeTextFile {
    name = "emacs-custom.el";
    text = ''
      ;; User specific info
      (setq user-full-name "${full-user-name}")

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
    ln -s ${emacs-custom} emacs-custom.el
    ln -s ${init-file} load-my-emacs-config.el
  '';

  installPhase = ''
    DIR=$out/share/emacs
    mkdir $DIR -p
    cp -r ../build "$DIR/site-lisp"
  '';
}
