{
  fetchurl,
  python,
  stdenv,
  writeScriptBin,
}:
let
  sha = "4e8e56fb655432bdbe9cae421be84b1a1cdc234e";
  src = fetchurl {
    url = "https://raw.githubusercontent.com/google/styleguide/${sha}/cpplint/cpplint.py";
    sha256 = "1r090kkph1lfw3l91apgfk00430v6lgva88q5ncxpr9jjagy9sik";
  };
in
writeScriptBin "cpplint"
''
  #!${stdenv.shell}
  ${python}/bin/python ${src} "$@"
''
