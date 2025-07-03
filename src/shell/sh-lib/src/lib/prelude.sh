# This is the standard prelude for sane scripts

## Standard prelude
##====================================================================
set -euo pipefail

readonly ARGS=("$@")
readonly NARGS="$#"
readonly PROGNAME="$(basename "$0")"
readonly PROGDIR="$( cd "$( dirname "$0" )" && pwd )"

## Functions
##====================================================================
fail() {
    local message="$1"
    {
        echo
        echo "$message"
        echo
    } >&2
    exit 1
}
