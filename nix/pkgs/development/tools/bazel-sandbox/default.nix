{
  bazel,
  docker,
  empty-builder,
  python,
  stdenv,
}:
stdenv.mkDerivation {
  src = ./.;
  name = "bazel-sandbox";
  buildInputs = [
    bazel
    docker
    python
  ];

  # Hack: Wheel doesn't like ancient timestamps
  SOURCE_DATE_EPOCH = "1471222800";

  builder = empty-builder;
}
