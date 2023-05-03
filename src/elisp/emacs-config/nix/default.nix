{
  emacs,
  stdenv,
}:

stdenv.mkDerivation rec {

  name = "emacs-config";
  src = ./../src;

  # Loading this file activates all the configuration
  init-file = ./load-my-emacs-config.el;

  buildInputs = [
    emacs
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
}
