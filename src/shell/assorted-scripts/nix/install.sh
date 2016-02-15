#!/usr/bin/env bash
# We just need to copy the scripts and the templates to the right place here. We
# ignore the install.sh script which is just intended for hackily "installing"
# the scripts when cloning the repo in a directory included in the PATH

source "$stdenv/setup"

readonly bin="$out/bin"
readonly share="$out/share/assorted-scripts"

mkdir -p "$bin"
mkdir -p "$share"

for i in $(find "$src" -maxdepth 1 -executable -type f); do
    cp "$i" "$bin"
done

cp -r "$src/git-hooks" "$share"
cp -r "$src/templates" "$share"
