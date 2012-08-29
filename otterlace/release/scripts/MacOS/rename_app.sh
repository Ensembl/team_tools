#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos_above_app.sh" || exit 1

old_app_name="$1"
new_app_name="$2"

if [ -z "${old_app_name}" ] || [ -z "${new_app_name}" ]; then
    echo "Usage: ${script_name} <old_app_name> <new_app_name>" >&2
    exit 3
fi

old_non_dist_name="$( versioned_non_dist "${old_app_name}" )" || exit $?
new_non_dist_name="${old_non_dist_name/${old_app_name}/${new_app_name}}" # retain versioning

mv -v "${old_app_name}" "${new_app_name}"
mv -v "${non_dist}/${old_non_dist_name}" "${non_dist}/${new_non_dist_name}"

remove_links "${new_app_name}"
make_links   "${new_app_name}" "${new_non_dist_name}"

# FIXME: if MacPorts is installed, it contains hard-coded paths.

exit 0

# EOF
