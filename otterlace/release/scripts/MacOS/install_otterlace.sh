#!/bin/bash

set -e # bail out on error

my_dir="$( dirname "$0" )"

. "${my_dir}/_macos_in_app.sh"  || exit 1
. "${my_dir}/_annotools_env.sh" || exit 2
perl_sanity_check || exit 3

ensembl_otter_dir="$1"
if [ ! -d "${ensembl_otter_dir}" ]; then
	echo "Usage: ${script_name} <ensembl_otter_dir>" >&2
	exit 4
fi

# Supporting script

for f in anacode_distro_code; do
    dest="${install_base}/bin/$f"
    cp -v "${etc_macos}/$f" "$dest"
    chmod +x "$dest"
done

hostname="$( hostname )"

var_otter="${install_base}/var/otter"
build_log_dir="${var_otter}/build_logs"
mkdir -v -p "${build_log_dir}"

set_stage_dir

generic_script_dir="${my_dir}/.."
otterlace_build="${my_dir}/../otterlace_build"

export PATH=$PATH:/usr/local/git/bin
pushd "${ensembl_otter_dir}"

otter_swac="${install_base}" \
build_log="${build_log_dir}" \
    "${generic_script_dir}/otterlace_build" --local-client-only "${stage_dir}"

. "${generic_script_dir}/_otterlace.sh"
version="$( full_version )"

popd

macos_dist_dir="${install_base}/otter/otter_rel${version}/ensembl-otter/scripts/MacOS/dist"

# App script
contents_dir="${app_base}/Contents"
macos_dir="${contents_dir}/MacOS"
mkdir -v -p "${macos_dir}"
cp -v "${macos_dist_dir}/otterlace" "${macos_dir}"

# Info.plist file
cp -v "${macos_dist_dir}/Info.plist" "${contents_dir}"

exit $?

