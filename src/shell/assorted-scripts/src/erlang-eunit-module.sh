#!/bin/sh
#
# Generates an erlang module template

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
TESTED_MODULE_NAME=${MODULE_NAME%_tests}

if [ ! -z "$COPYRIGHT_HOLDER" ]; then
    echo "%%% @copyright $YEAR $COPYRIGHT_HOLDER" > $DEST
fi

sed -e "s/@@TESTED_MODULE@@/$TESTED_MODULE_NAME/g;s/@@MODULE@@/$MODULE_NAME/g" \
    < $TEMPLATES/erlang-eunit-module.template \
    >> $DEST

echo
echo "Created $DEST!"
echo
