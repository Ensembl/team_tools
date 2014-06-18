#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos_above_app.sh" || exit 1

usage() {
    local problem
    problem=''
    [ -n "$1" ] && problem="\n${script_name}: ${1}\n"

    bail "Syntax:  [-f] [-n] <app_name>
${problem}"
}

dry_run=
force=

while [ $# -gt 0 ] && [ "${1:0:1}" = "-" ]; do
    sw=$1
    shift
    case $sw in
        -h | --help)
            usage
            ;;
        -f | --force)
            force=-f
            ;;
        -n | --dry-run)
            dry_run=1
            ;;
        *)
            usage "unknown option $sw"
            ;;
    esac
done

app_name="${1%/}"           # strip trailing slash

if [ -z "${app_name}" ]; then
    usage "<app_name> missing"
fi

non_dist_name="$( versioned_non_dist "${app_name}" )" || echo "Cannot find non_dist for '${app_name}" >&2

[ -z "$dry_run" ] && set -x     # echo the actual commands if not a dry run

[ -n "$non_dist_name" ] && ${dry_run:+echo DRY_RUN:} rm -r $force "${non_dist}/${non_dist_name}"
${dry_run:+echo DRY_RUN:} rm -r $force "${app_name}"

exit 0

# EOF
