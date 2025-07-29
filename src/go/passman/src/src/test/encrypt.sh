#!/usr/bin/env bash
#
# This script requires next environmet variables to be set
#  * SH_LIB: the lib directory to load utils from

source $SH_LIB/prelude.sh

if [[ "$NARGS" != 1 ]]; then
    echo "uasge: $PROGNAME"
fi


# use script to inject the password with a pseudo terminal

cd "$PROGDIR"
echo "Use foobar as passphrase"

age --encrypt --passphrase < testfile.sexp > testfile.age
