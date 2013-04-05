#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos_above_app.sh" || exit 1

app_name="${1%/}"               # strip trailing slash

if [ -z "${app_name}" ] ; then
    echo "Usage: ${script_name} <app_name>" >&2
    exit 3
fi

non_dist_name="$( old_links_versioned_non_dist "${app_name}" )" || exit $?

old_remove_links "${app_name}"
make_links       "${app_name}" "${non_dist_name}"

exit 0

# EOF
