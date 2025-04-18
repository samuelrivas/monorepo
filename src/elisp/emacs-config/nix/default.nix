{
  emacs,
  stdenv,
}: let
  emacsPackages = emacs.pkgs;

  # We need some packages to be available at compile time
  emacsWithPackages = emacs.pkgs.emacsWithPackages (with emacsPackages; [
    helm
  ]);
in
  stdenv.mkDerivation rec {
    name = "emacs-config";
    src = ./../src;

    # Loading this file activates all the configuration
    init-file = ./load-my-emacs-config.el;

    buildInputs = [
      emacsWithPackages
    ];

    preBuild = ''
      ln -s ${init-file} load-my-emacs-config.el
    '';

    installPhase = ''
      DIR=$out/share/emacs
      mkdir $DIR -p
      cp -r ../build "$DIR/site-lisp"
      cp *.el "$DIR/site-lisp"
    '';

    passthru.config-file = ../src/emacs-config.el;
  }
