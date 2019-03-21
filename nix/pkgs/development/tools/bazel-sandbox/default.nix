{
  bazel,
  empty-builder,
  stdenv,
}:
stdenv.mkDerivation {
  src = ./.;
  name = "bazel-sandbox";
  buildInputs = [
    bazel
  ];
  builder = empty-builder;
}
