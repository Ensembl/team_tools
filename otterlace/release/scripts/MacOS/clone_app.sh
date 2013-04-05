#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos_above_app.sh" || exit 1

source_app="$1"
target_app="$2"

if [ -z "${source_app}" ] || [ -z "${target_app}" ]; then
    echo "Usage: ${script_name} <source_app> <target_app>" >&2
    exit 3
fi

save_links() {
    local install_base
    install_base="$1"

    pushd "${install_base}" > /dev/null

    mkdir "_saved_links"
    for f in share var; do
        mv $f "_saved_links/"
    done

    popd > /dev/null
}

restore_links() {
    local install_base
    install_base="$1"

    pushd "${install_base}" > /dev/null

    for f in share var; do
        rm "$f"
        mv "_saved_links/$f" "$f"
    done
    rmdir "_saved_links"

    popd > /dev/null
}

source_non_dist="${non_dist}/$( versioned_non_dist "${source_app}" )" || exit $?

"${macos_scripts}/setup_app_skeleton.sh" "${target_app}"

target_non_dist="${non_dist}/$( versioned_non_dist "${target_app}" )" || exit $?

source_install_base="${source_app}/${resources_path}"
target_install_base="${target_app}/${resources_path}"

source_macos="${source_app}/${contents_macos_path}"
target_macos="${target_app}/${contents_macos_path}"

echo "Saving away target links"
save_links "${target_install_base}"

echo "Cleaning target non_dist"
rm -r ${target_non_dist}/{share,var}

echo "Copying main contents..."
cp -a ${source_install_base}/* "${target_install_base}/"

echo "Copying ${contents_macos_path}..."
mkdir -v -p "${target_macos}"
cp -a ${source_macos}/* "${target_macos}/" || echo "No ${source_macos} - skipping on...."

copy_non_dist "${source_non_dist}" "${target_non_dist}" 'exclude_macports_build'

echo "Restoring target links"
restore_links "${target_install_base}"

echo "Done."

exit 0

# EOF
