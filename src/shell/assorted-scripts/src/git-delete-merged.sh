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
    echo "Usage ${PROGNAME} <local|all> <our-remote> <upstream-branch>" >&2
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

delete_remote_branch() {
    local long_name="$1"
    local remote="$(echo "$long_name" | cut -d/ -f1)"
    local branch="$(echo "$long_name" | cut -d/ -f2)"
    local sha="$(git rev-parse "$long_name")"

    echo "Deleting remote $sha ($long_name)"
    if ! git push "$remote" :"$branch"; then
        echo "Couldn't delete remote branch: $long_name"
    fi
}

delete_local_branch() {
    local long_name="$1"
    local branch="$(echo "$long_name" | cut -d/ -f2)"
    local sha="$(git rev-parse "$long_name")"

    echo "Deleting local $sha ($branch)"
    if ! git branch -D "$branch"; then
        echo "Couldn't delete local branch: $branch"
    fi
}

## Main
##====================================================================
main() {
    (( $NARGS != 3 )) && usage_and_exit
    local remove_mode upstream_branch our_remote merged branch

    remove_mode="${ARGS[0]}"
    our_remote="${ARGS[1]}"
    upstream_branch="${ARGS[2]}"
    merged=("$(get_merged "$our_remote" "$upstream_branch")")

    if [[ "$remove_mode" != "local" && "$remove_mode" != "all" ]]; then
        usage_and_exit
    fi

    for branch in ${merged[@]}; do
        if [[ ! "$branch" =~ "/master" ]]; then
            delete_local_branch "$branch"
            if [[ "$remove_mode" == "all" ]]; then
                delete_remote_branch "$branch"
            fi
        fi
    done
}

main
