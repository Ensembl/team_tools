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

hostname="$( hostname -f )"

var_otter="${install_base}/var/otter"
mkdir -v -p "${var_otter}"
build_log="${var_otter}/build_log.yaml"

set_stage_dir

otterlace_build="${my_dir}/../otterlace_build"
(
    export PATH=$PATH:/usr/local/git/bin &&
    cd "${ensembl_otter_dir}" && 
    \
	otterlace_client_host="${hostname}" \
	otter_swac="${install_base}" \
	build_log="${build_log}" \
	zmap_build="${stage_dir}" \
	"${otterlace_build}"
)

exit $?

