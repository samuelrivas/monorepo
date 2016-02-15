#!/usr/bin/env bash
#
# This script finds all branches that have been merged to the provided (remote)
# branch and deletes the remote and local copy of them
#
# Just in case of disaster, you'll get a printout with the affected sha's, so
# it is fairly easy to recover a branch in case you didn't really wanted to
# delete it

## Standard prelude
##====================================================================
set -e
set -u

readonly ARGS=("$@")
readonly NARGS="$#"
readonly PROGNAME="$(basename "$0")"

## Functions
##====================================================================
usage() {
    echo "Usage ${PROGNAME} <our-remote> <upstream-branch>" >&2
}

usage_and_exit() {
    usage
    exit 1
}

get_merged() {
    local our_remote="$1"
    local upstream_branch="$2"
    git branch --list "$our_remote/*" \
        --remotes                     \
        --merged "$upstream_branch"
}

delete_branch() {
    local long_name="$1"
    local remote="$(echo "$long_name" | cut -d/ -f1)"
    local branch="$(echo "$long_name" | cut -d/ -f2)"
    local sha="$(git rev-parse "$long_name")"

    echo "Deleting $sha ($long_name)"
    git push "$remote" :"$branch"
    git branch -D "$branch"
}

## Main
##====================================================================
main() {
    (( $NARGS != 2 )) && usage_and_exit
    local upstream_branch our_remote merged branch

    our_remote="${ARGS[0]}"
    upstream_branch="${ARGS[1]}"
    merged=("$(get_merged "$our_remote" "$upstream_branch")")

    for branch in ${merged[@]}; do
        if [[ ! "$branch" =~ "/master" ]]; then
            delete_branch "$branch"
            echo $branch
        fi
    done
}

main
