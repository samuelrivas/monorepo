#!/bin/sh
#
# Create a version tag and do some sanity checks
#
# The workflow behind this script is as follows:
#
#  * There are two branches for which the published history cannot change:
#    master and develop
#  * master always points to the last tag, unless there is not such a tag, then
#    to "somewhere" in the past history of develop
#  * develop contains master, and is allowed to point to a non-tagged commit
#  * Neither master nor develop are allowed to have merge commits, there must
#    be a single way back in history
#  * Any other branch is considered temporary, there are not any restrictions
#    imposed on them
#  * Version tags cannot change or be deleted once published. An erroneous tag
#    must be fixed committing the fixes on top of it
#  * Tags must be signed with the tagger public key
#
# This scripts does not fully enforce that workflow, but does some checks to
# avoid common mistakes:
#
#  * master and origin/master must be the same (arguably, we could fetch
#    origin/master to be sure, but we want this script to be as local as
#    possible)
#  * The tag must be done in a branch called develop
#  * There must not be uncommited changes in develop further than those
#    explicitly ignored
#  * There must be a master branch that can be fast-forwarded to develop
#  * The user.email key must be properly configured and there must be gpg a
#    public key associated to it
#
# More tests could be added in future versions of this script

set -e

if [ $# -ne 1 ]; then
    echo
    echo "Usage $0 <version>";
    echo
    exit 1;
fi

error () {
    echo
    echo $1
    echo
    exit 1
}

MASTER_BRANCH=master
DEVELOP_BRANCH=develop
VERSION=$1

# Check that we are in the right branch
FULL_BRANCH=$(git symbolic-ref -q HEAD)  \
    || error "You are in a detached head.  \
              Checkout $DEVELOP_BRANCH branch before tagging"

BRANCH=${FULL_BRANCH#refs/heads/}

if [ $BRANCH != $DEVELOP_BRANCH ]; then
    error "You are in the $BRANCH branch. \
           Checkout $DEVELOP_BRANCH before tagging"
fi

# Check that we are in a clean repository
STATUS=`git status --porcelain`
if [ ! -z "$STATUS" ]; then
    error "There are ucommited changes, commit them before tagging";
fi

# Check that master is updated
MASTER=`git rev-parse master`
ORIGIN_MASTER=`git rev-parse origin/master`

if [ "$MASTER" != "$ORIGIN_MASTER" ]; then
    error "master doesn't point ot origin/master. Update before tagging"
fi

# Check that we can fast-forward master to HEAD
UNMERGED_COMMITS=$(git rev-list $DEVELOP_BRANCH..$MASTER_BRANCH)
if [ ! -z "$UNMERGED_COMMITS" ]; then
    error "This is a mess, there are unmerged commits in $MASTER_BRANCH! \
           $DEVELOP_BRANCH has diverged, fix it before tagging"
fi

# Tag and sign
#
# Get the email from git config, as it will try to find a key for "<user name>
# <email>" and that might not be the key identifier
EMAIL=$(git config --get user.email)
git tag -a -u $EMAIL $VERSION

# Merge master
echo
echo "Forwarding $MASTER_BRANCH to $VERSION"

git checkout -q $MASTER_BRANCH
git merge -q --ff-only $VERSION
git checkout -q $DEVELOP_BRANCH

echo
echo "Success!"
echo "Remember to push this tag to upstream"
echo
