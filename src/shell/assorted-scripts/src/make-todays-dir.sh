#!/bin/bash

set -e
set -u

if (( $# == 1 )); then
    readonly SUFFIX=""
elif (( $# == 2 )); then
    readonly SUFFIX="-$2"
else
    echo
    echo "Usage: $(basename "$0") <dest> [suffix]"
    echo

    exit 1
fi

readonly SRC="$1"
readonly TODAY="$(date '+%Y-%m-%d')"
readonly DEST="$SRC/$TODAY$SUFFIX"

mkdir "$DEST"
