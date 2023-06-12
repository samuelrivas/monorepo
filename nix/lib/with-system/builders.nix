{
  writeScript,
}:

rec {
  # Generate an empty $out, useful for derivations where we care only about
  # sandboxing
  empty-builder = writeScript "empty-builder" ''
    source "$stdenv/setup"
    mkdir "$out"
  '';

  # A trivial builder for java applications that just copy a jar file from the
  # Internet
  simple-java-builder = writeScript "java-builder" ''
    source "$stdenv/setup"

    mkdir -p "$out/share/java"
    ln -s "$src" "$out/share/java/$name.jar"
  '';
}
