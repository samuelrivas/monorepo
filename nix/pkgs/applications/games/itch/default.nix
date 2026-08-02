# Feels very wrong, but we can play Dwarf Fortress it we tell it where to find
# the SDL libraries
{
  itch,
  writeShellScriptBin,
  SDL2,
  SDL2_image,
}:
writeShellScriptBin "itch"
''
  LD_LIBRARY_PATH=${SDL2}/lib:${SDL2_image}/lib
  export LD_LIBRARY_PATH
  exec ${itch}/bin/itch
''
