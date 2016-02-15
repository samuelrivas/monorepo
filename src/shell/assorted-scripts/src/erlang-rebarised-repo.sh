#!/bin/sh
#
# Generates an erlang header file template

set -e

SCRIPT_HOME="$( cd "$( dirname "$0" )" && pwd )"
TEMPLATES="$SCRIPT_HOME/assorted-scripts/templates"
DEST=$1

if [ -z "$DEST" ]; then
    echo
    echo "usage: `basename $0` <dir>"
    echo
    exit 1
fi

APP_NAME=`basename $DEST`

# --------------------------------------------------------------------
# Create directory structure
# --------------------------------------------------------------------
mkdir $DEST

for i in src priv ebin include deps doc; do
    mkdir $DEST/$i;
    touch $DEST/$i/.gitignore;
done

# --------------------------------------------------------------------
# Create standard gitignore
# --------------------------------------------------------------------
cp $TEMPLATES/erlang-rebarised-gitignore $DEST/.gitignore

# --------------------------------------------------------------------
# Create standard rebar.config
# --------------------------------------------------------------------
cp $TEMPLATES/rebar.config $DEST/rebar.config

# --------------------------------------------------------------------
# Create standard Makefile
# --------------------------------------------------------------------
cp $TEMPLATES/erlang-rebarised-makefile $DEST/Makefile

# --------------------------------------------------------------------
# Scripts for CI systems
# --------------------------------------------------------------------
cp $TEMPLATES/erlang-rebarised-build-and-test.sh $DEST/build-and-test.sh
chmod u+x $DEST/build-and-test.sh

# --------------------------------------------------------------------
# app.src
# --------------------------------------------------------------------
sed -e "s/@@APP_NAME@@/$APP_NAME/g" \
    < $TEMPLATES/erlang-app-src.template \
    > $DEST/src/$APP_NAME.app.src

# --------------------------------------------------------------------
# overview.edoc
# --------------------------------------------------------------------
sed -e "s/@@APP_NAME@@/$APP_NAME/g" \
    < $TEMPLATES/erlang-overview-edoc.template \
    > $DEST/doc/overview.edoc

# --------------------------------------------------------------------
# Git init and initial commit
# --------------------------------------------------------------------
OLDPWD=$PWD
cd $DEST
git init --quiet
git add .
git commit -m "Initial commit"  --quiet
cd $OLDPWD

# --------------------------------------------------------------------
# Some remarks
# --------------------------------------------------------------------
echo
echo "You have your new app in $DEST. Just remember:"
echo "  * To edit the app.src file"
echo "  * To edit the rebar.config if you want"
echo "  * To edit the rebar version in the Makefile if you want"
echo
echo "Now go start coding"
echo
