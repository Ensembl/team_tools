#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos.sh"         || exit 1
. "$( dirname "$0" )/_annotools_env.sh" || exit 2

check_set_zmap_dist_dir_from_arg "$@"
goto_build_root

echo "In ${PWD}"

# ZMap

zmap_tarball="${zmap_dist_dir}/zmap-*.tar.gz"
unpack_matching_tarball "${zmap_tarball}" "zmap-"
zmap_src="${unpacked_tarball}"
config_make "${zmap_src}"

# Seqtools

seqtools_tarball="${zmap_dist_dir}/seqtools-*.tar.gz"
unpack_matching_tarball "${seqtools_tarball}" "seqtools-"
seqtools_src="${unpacked_tarball}"
config_make "${seqtools_src}"

exit 0
