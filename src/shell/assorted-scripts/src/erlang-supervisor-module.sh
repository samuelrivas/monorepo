#!/bin/sh
#
# Generates an erlang supervisor template

set -e

SCRIPT_HOME="$( cd "$( dirname "$0" )" && pwd )"
TEMPLATES="$SCRIPT_HOME/assorted-scripts/templates"
DEST=$1
COPYRIGHT_HOLDER=$2

if [ -z "$DEST" ]; then
    echo
    echo "usage: `basename $0` <file.erl> [\"<copyright holder>\"]"
    echo
    exit 1
fi

YEAR=`date +%Y`
FILENAME=`basename $DEST`
MODULE_NAME=${FILENAME%.erl}

if [ ! -z "$COPYRIGHT_HOLDER" ]; then
    echo "%%% @copyright $YEAR $COPYRIGHT_HOLDER" > $DEST
fi

sed -e "s/@@MODULE@@/$MODULE_NAME/g" \
    < $TEMPLATES/erlang-supervisor-module.template \
    >> $DEST

echo
echo "Created $DEST!"
echo
