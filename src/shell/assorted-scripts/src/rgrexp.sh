#!/bin/sh

if [ 2 -ne $# -a 3 -ne $# ]; then
    echo
    echo "Usage $0 <pattern> <file-pattern> [<dir>]"
    echo
    exit
fi

PATTERN=$1
shift
FILES=$1
shift
DIR=$1

if [ -z "$DIR" ]; then
    DIR="."
fi

find -L $DIR -name "$FILES" -type f | xargs -I{} grep -H --color "$PATTERN" "{}"

