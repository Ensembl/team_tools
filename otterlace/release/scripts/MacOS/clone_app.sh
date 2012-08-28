#!/bin/bash

set -e # bail out on error

# We don't include _macos.sh as it assumes we're running in the app directory
# . "$( dirname "$0" )/_macos.sh" || exit 1

source_app="$1"
target_app="$2"

macos_scripts="$( dirname "$0" )"
script_name="$( basename "$0" )"

if [ -z "${source_app}" ] || [ -z "${target_app}" ]; then
    echo "Usage: ${script_name} <source_app> <target_app>" >&2
    exit 3
fi

versioned_non_dist() {
    local my_non_dist_link my_non_dist_dir

    my_non_dist_link="$1/_my_non_dist"
    if [ ! -e "${my_non_dist_link}" ]; then echo "Cannot find '${my_non_dist_link}'" >&2; exit 4; fi

    my_non_dist_dir="$( cd "${my_non_dist_link}" 2>/dev/null && pwd -P )"
    if [ -z "${my_non_dist_dir}" ]; then echo "Cannot resolve link for '${my_non_dist_link}'" >&2; exit 5; fi

    echo "_non_dist/$( basename "${my_non_dist_dir}" )"
}

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

source_non_dist="$( versioned_non_dist "${source_app}" )" || exit $?

"${macos_scripts}/setup_app_skeleton.sh" "${target_app}"

target_non_dist="$( versioned_non_dist "${target_app}" )" || exit $?

resource_path="Contents/Resources"
source_install_base="${source_app}/${resource_path}"
target_install_base="${target_app}/${resource_path}"

echo "Saving away target links"
save_links "${target_install_base}"

echo "Cleaning target non_dist"
rm -r ${target_non_dist}/{share,var}

echo "Copying main contents..."
cp -a ${source_install_base}/* "${target_install_base}/"

# FIXME: should probably be cleaning macports build dir by default.
echo "Copying non_dist contents: ${source_non_dist} => ${target_non_dist} ..."
rsync -a --exclude '/var/macports/build/*' ${source_non_dist}/{share,var} "${target_non_dist}/"

echo "Restoring target links"
restore_links "${target_install_base}"

echo "Done."

exit 0

# EOF
