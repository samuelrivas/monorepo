#!/bin/sh
#
# This deletes everything that is not under version control with the exception
# of those files thate are listed in ~/.clean-git-repo.save-files"

set -e

SAVE_FILES_FILE="$HOME/.clean-git-repo.save-files"

if [ -f $SAVE_FILES_FILE ]; then
    TEMP=$(mktemp -d)
    for i in $(cat $SAVE_FILES_FILE); do
        [ -f $i ] && (echo "** saving    $i"; mv $i $TEMP)
    done
fi

git clean -dffx
git submodule foreach --recursive git clean -dfx

if [ -f $SAVE_FILES_FILE ]; then
    for i in $(cat $SAVE_FILES_FILE); do
        [ -f $TEMP/$i ] && (echo "** restoring $i"; cp $TEMP/$i $i)
    done
    rm -rf $TEMP
fi
