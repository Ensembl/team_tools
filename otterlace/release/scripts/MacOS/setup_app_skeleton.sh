#!/bin/bash

set -e # bail out on error

# We don't include _macos.sh as it assumes we're running in the app directory
# . "$( dirname "$0" )/_macos.sh" || exit 1

new_app="$1"
if [ -z "${new_app}" ]; then
    me="$( basename "$0" )"
    echo "Usage: $me <app_name.app>" >&2
    exit 3
fi

# Rejig to move var/ and share/ out of the tree, and then to share within them

non_dist="_non_dist"
shared_non_dist="_shared"

dist_dir="${PWD}"
app_base="${dist_dir}/${new_app}"

non_dist_path="${dist_dir}/${non_dist}"
my_non_dist_path="$non_dist_path/${new_app}"
shared_non_dist_path="${non_dist_path}/${shared_non_dist}"

echo "About to make: ${app_base}"
echo " and links to: ${my_non_dist_path}"
echo
echo "<Ctrl-C> now if that looks wrong"
echo
sleep 5

resource_path="Contents/Resources"
install_base="${app_base}/${resource_path}"

mkdir -v -p "${install_base}"

mkdir -v -p "${shared_non_dist_path}/var/macports/distfiles"

mkdir -v -p "${my_non_dist_path}/share"
mkdir -v -p "${my_non_dist_path}/var"
mkdir -v -p "${my_non_dist_path}/var/macports"

# $PWD/ <-------------------\
#   $new_app/ <----------\  |
#     Contents <------\  |  |
rel_inst_to_non_dist="../../../${non_dist}"
(
    cd "${install_base}" 
    echo "In ${PWD}:"
    for f in var share; do
	ln -v -sf "${rel_inst_to_non_dist}/${new_app}/$f" .
    done
)

(
    cd "${my_non_dist_path}/var/macports"
    echo "In ${PWD}:"
    ln -v -sf "../../../${shared_non_dist}/var/macports/distfiles" .
)

exit 0

# EOF