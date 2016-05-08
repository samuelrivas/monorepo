## Just collect the dictionaries I want so that I can configure aspell to use
## them

{
  aspell,
  aspellDicts,
  makeWrapper,
  stdenv,
  symlinkJoin,
  writeScript,
  writeText,
}:

stdenv.mkDerivation rec {
  name = "aspell-wrapped-0.0.0";
  dicts = symlinkJoin "dicts"
    [
      aspellDicts.en
      aspellDicts.es
      aspellDicts.sv
    ];

  conf_file = writeText "aspell-conf" "dict-dir ${dicts}/lib/aspell";

  buildInputs = [
    makeWrapper
  ];

  inherit aspell;

  builder = writeScript "builder.sh" ''
    source $stdenv/setup

    mkdir $out/bin -p
    makeWrapper $aspell/bin/aspell $out/bin/aspell \
      --suffix "ASPELL_CONF" ";" "'per-conf $conf_file'"
   '';
}
