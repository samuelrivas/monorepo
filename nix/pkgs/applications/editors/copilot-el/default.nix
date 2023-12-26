{
  dash,
  editorconfig,
  emacs,
  fetchFromGitHub,
  nodejs,
  s,
  trivialBuild,
}:
trivialBuild {
  pname = "copilot-el";
  version = "unstable-2023-12-26";
  src = fetchFromGitHub {
    owner = "zerolfx";
    repo = "copilot.el";
    rev = "d4fa14cea818e041b4a536c5052cf6d28c7223d7";
    sha256 = "sha256-Tzs0Dawqa+OD0RSsf66ORbH6MdBp7BMXX7z+5UuNwq4=";
  };
  packageRequires = [
    dash
    editorconfig
    nodejs
    s
  ];
  postInstall = ''
    cp -r $src/dist $LISPDIR
  '';
}
