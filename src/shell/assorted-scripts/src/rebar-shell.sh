#!/bin/sh
#
# Starts an erlang shell with the path set for a rebarised app

set -e

# Find the deps dir (heuristically)
DEPS_CANDIDATES="lib deps"
DEPS=""
for i in $DEPS_CANDIDATES; do
    if [ -d "$i" ]; then
        DEPS=$i
    fi
done

if [ -z "$DEPS" ]; then
    echo
    echo "Cannot find the dependencies in any of: $DEPS_CANDIDATES"
    echo
    exit 1
fi

ERL_LIBS=$PWD/$DEPS erl -pa $PWD/ebin $PWD/test $PWD/test/ebin $@
