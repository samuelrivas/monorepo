## Just collect the dictionaries I want so that I can configure aspell to use
## them

{
  aspell,
  aspellDicts,
  stdenv,
  symlinkJoin,
  writeScript,
  makeWrapper,
}:

stdenv.mkDerivation rec {
  name = "aspell-wrapped-0.0.0";
  dicts = symlinkJoin "dicts"
    [
      aspellDicts.en
      aspellDicts.es
      aspellDicts.sv
    ];

  buildInputs = [
    makeWrapper
  ];

  inherit aspell;

  builder = writeScript "builder.sh" ''
    source $stdenv/setup

    mkdir $out/bin -p
    makeWrapper $aspell/bin/aspell $out/bin/aspell \
      --set ASPELL_CONF "'dict-dir $dicts/lib/aspell'"
   '';
}
