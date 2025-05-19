{
  buildGoModule,
  go,
}:
buildGoModule {
  pname = "passman-go";
  version = "1.0.0";

  src = ../src/src;

  vendorHash = null;

  nativeBuildInputs = [go];
}
