#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos_above_app.sh" || exit 1

source_app="${1%/}"             # strip trailing slash
target_app="${2%/}"             # --"--

if [ -z "${source_app}" ] || [ -z "${target_app}" ]; then
    echo "Usage: ${script_name} <source_app> <target_app>" >&2
    exit 3
fi

source_non_dist="${non_dist}/$( versioned_non_dist "${source_app}" )" || exit $?

"${macos_scripts}/setup_app_skeleton.sh" "${target_app}"

target_non_dist_name="$( versioned_non_dist "${target_app}" )" || exit $?
target_non_dist_path="${non_dist}/${target_non_dist_name}"

source_install_base="${source_app}/${resources_path}"
target_install_base="${target_app}/${resources_path}"

source_macos="${source_app}/${contents_macos_path}"
target_macos="${target_app}/${contents_macos_path}"

echo "Removing empty target Resources contents"
rm -r ${target_install_base}/*

echo "Copying main contents..."
cp -pR ${source_install_base}/* "${target_install_base}/"

echo "Copying ${contents_macos_path}..."
mkdir -v -p "${target_macos}"
cp -pR ${source_macos}/* "${target_macos}/" || echo "No ${source_macos} - skipping on...."

copy_non_dist "${source_non_dist}" "${target_non_dist_path}" 'exclude_macports_build'

echo "Removing copied target links"
remove_links "${target_app}"

echo "Creating correct target links"
make_links "${target_app}" "${target_non_dist_name}"

echo "Done."

exit 0

# EOF
