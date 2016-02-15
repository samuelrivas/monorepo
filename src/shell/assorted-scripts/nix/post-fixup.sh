#!/usr/bin/env bash
#
# The scripts that use templates expect the templates to be in the templates
# directory at the same directory the scripts live. This is not very elegant for
# packetising, so we install the templates in the share directory. This step
# rewrites the references to those scripts so that they remain valid.
#
# We also patch some direct calls to binaries so that they point to the store instead
#
# Ideally we should have a less brittle way of configuring assorted scripts, now
# that we can install them

source "$stdenv/setup"

fix_templates_dir () {
    local file="$1"

    substituteInPlace                                       \
        "$file"                                             \
        --replace '$SCRIPT_HOME/assorted-scripts/templates' \
        "$out/share/assorted-scripts/templates"
}

wrap_calls () {
    local program="$1"
    local dependencies="$2"

    wrapProgram "$out/bin/$program" --suffix-each PATH : "$dependencies"
}

main() {
    local templated_script;
    local templated_scripts;

    templated_scripts=( $(grep -l templates *.sh) )

    for templated_script in "${templated_scripts[@]}"; do
        echo "patching templates dir in $templated_script"
        fix_templates_dir "$out/bin/$templated_script"
    done

    # Many scripts need to be added here
    wrap_calls "screen-control.sh" "$xset/bin $xbacklight/bin $xrandr/bin"
    wrap_calls "absolute-which" "$which/bin $gawk/bin"
    wrap_calls "join-pdf.sh" "$pdftk/bin"
    wrap_calls "pp-json.sh" "$python/bin"
    wrap_calls "selection-to-clipboard" "$xclip/bin"
}

main
