#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos_above_app.sh" || exit 1

new_app="$1"
if [ -z "${new_app}" ]; then
    echo "Usage: ${script_name} <app_name.app>" >&2
    exit 3
fi

if [ -e "${new_app}" ]; then
    echo "App directory '${new_app}' already exists" >&2
    exit 4
fi

# Rejig to move var/ and share/ out of the tree, and then to share within them

non_dist="_non_dist"
shared_non_dist="_shared"

dist_dir="${PWD}"
app_base="${dist_dir}/${new_app}"

serial="$(date +%s).$$"
new_app_versioned="${new_app}.${serial}"

non_dist_path="${dist_dir}/${non_dist}"
my_non_dist_path="$non_dist_path/${new_app_versioned}"
shared_non_dist_path="${non_dist_path}/${shared_non_dist}"

echo "About to make: ${app_base}"
echo " and links to: ${my_non_dist_path}"
echo
echo "<Ctrl-C> now if that looks wrong"
echo
sleep 5

install_base="${app_base}/${resources_path}"

mkdir -v -p "${install_base}"

mkdir -v -p "${shared_non_dist_path}/var/macports/distfiles"

mkdir -v -p "${my_non_dist_path}/share"
mkdir -v -p "${my_non_dist_path}/var"
mkdir -v -p "${my_non_dist_path}/var/macports"

(
    cd "${app_base}"
    echo "In ${PWD}:"
    ln -v -sf "../${non_dist}/${new_app_versioned}" "_my_non_dist"
)

# $PWD/ <-------------------\
#   $new_app/ <----------\  |
#     Contents <------\  |  |
rel_inst_to_non_dist="../../../${non_dist}"
(
    cd "${install_base}"
    echo "In ${PWD}:"
    for f in var share; do
	ln -v -sf "${rel_inst_to_non_dist}/${new_app_versioned}/$f" .
    done
)

(
    cd "${my_non_dist_path}/var/macports"
    echo "In ${PWD}:"
    ln -v -sf "../../../${shared_non_dist}/var/macports/distfiles" .
)

exit 0

# EOF
