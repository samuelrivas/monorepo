#!/usr/bin/env bash
#
# Script to do incremental backups with arbitrary retention. You should build
# scripts on top of this one to avoid running it with different parameters for
# the same backup
#
# This script is heavily inspired by this post by Jeff Layton:
#   http://www.admin-magazine.com/Articles/Using-rsync-for-Backups

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
    cat >&2 <<EOF

Usage $PROGNAME <src-dir> <dest-dir> <retention>

<retention> is an integer representing the the amount of increments we want to
save, 0 meaning no increments.

The backup is done in <dest-dir>/<src-dir>.backup, each increment is stored as
<dest-dir>/<src-dir>.N, where N is the number of the increment. Increment 1 is
the closest in time to the current backup

EOF
}

usage_and_exit() {
    usage
    exit 1
}

parse_args() {
    local args=("$@")

    (( "${#args[@]}" == 3 )) \
        || usage_and_exit

    readonly SRC_DIR="${args[0]}"
    readonly DEST_DIR="${args[1]}"
    readonly RETENTION="${args[2]}"

    [[ "$RETENTION" =~ ^-?[0-9]+$ ]] \
        || usage_and_exit
}

move_if_exists() {
    local src="$1"
    local dst="$2"

    if [[ -e "$1" ]]; then
        mv "$1" "$2"
    fi
}

push_old_backups() {
    local src_basename="$1"
    local dest_dir="$2"
    local retention="$3"
    local i

    rm -rf "$dest_dir/$src_basename.$retention"

    for i in $(seq "$retention" -1 2); do
        local previous="$dest_dir/$src_basename.$(($i - 1))"
        local current="$dest_dir/$src_basename.$i"

        move_if_exists "$previous" "$current"
    done

    move_if_exists "$dest_dir/$src_basename.backup" "$dest_dir/$src_basename.1"
}

backup() {
    local src_dir="$1"
    local dest_dir="$2"
    local retention="$3"
    local src_basename="$(basename "$src_dir")"
    local maybe_link=""

    if (( "$retention" > 0 )); then
        push_old_backups "$src_basename" "$dest_dir" "$retention"
        maybe_link="--link-dest=$dest_dir/$src_basename.1"

        rsync -avh                           \
            --delete                         \
            "$maybe_link"                    \
            "$src_dir/"                      \
            "$dest_dir/$src_basename.backup"
    else
        rsync -avh                           \
            --delete                         \
            "$src_dir/"                      \
            "$dest_dir/$src_basename.backup"
    fi
}

## Main
##====================================================================
main() {
    parse_args "${ARGS[@]:-}"
    backup "$SRC_DIR" "$DEST_DIR" "$RETENTION"
}

main
