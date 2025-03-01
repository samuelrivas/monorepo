{
  HSH,
  haskell-pkg,
  lib,
  libnotify,
  makeWrapper,
  perlude,
}:
haskell-pkg {
  name = "low-battery";
  src = ./../src;
  haskell-libs = [
    HSH
    perlude
  ];
  extra-native-build-inputs = [libnotify];
  extra-drv = {meta.platforms = lib.platforms.linux;};

  extra-build-inputs = [makeWrapper];

  extra-drv = {
    postFixup = ''
      wrapProgram "$out/bin/low-battery-check" \
        --prefix PATH : ${libnotify}/bin
    '';
  };
}
