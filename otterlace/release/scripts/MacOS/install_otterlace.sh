#!/bin/bash

set -e # bail out on error

my_dir="$( dirname "$0" )"

. "${my_dir}/_macos.sh"         || exit 1
. "${my_dir}/_annotools_env.sh" || exit 2
perl_sanity_check || exit 3

ensembl_otter_dir="$1"
if [ ! -d "${ensembl_otter_dir}" ]; then
	me="$( basename "$0" )"
	echo "Usage: $me <ensembl_otter_dir>" >&2
	exit 4
fi

# Supporting script

for f in anacode_distro_code; do
    dest="${install_base}/bin/$f"
    cp -v "${etc_macos}/$f" "$dest"
    chmod +x "$dest"
done

hostname="$( hostname -f )"

var_otter="${install_base}/var/otter"
build_log_dir="${var_otter}/build_logs"
mkdir -v -p "${build_log_dir}"

set_stage_dir

otterlace_build="${my_dir}/../otterlace_build"
(
    export PATH=$PATH:/usr/local/git/bin &&
    cd "${ensembl_otter_dir}" && 
    \
	otterlace_client_host="${hostname}" \
	otter_swac="${install_base}" \
	build_log="${build_log_dir}" \
	zmap_build="${stage_dir}" \
	"${otterlace_build}"
)

exit $?

