#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos_above_app.sh" || exit 1

old_app_name="$1"
new_app_name="$2"

if [ -z "${old_app_name}" ] || [ -z "${new_app_name}" ]; then
    echo "Usage: ${script_name} <old_app_name> <new_app_name>" >&2
    exit 3
fi

old_non_dist="$( versioned_non_dist "${old_app_name}" )" || exit $?
new_non_dist="${old_non_dist/${old_app_name}/${new_app_name}}" # retain versioning
new_non_dist_name="${new_non_dist#${non_dist}/}"               # strip prefixed path

mv -v "${old_app_name}" "${new_app_name}"
mv -v "${old_non_dist}" "${new_non_dist}"

remove_links "${new_app_name}"
make_links   "${new_app_name}" "${new_non_dist_name}"

# FIXME: if MacPorts is installed, it contains hard-coded paths.

exit 0

# EOF
