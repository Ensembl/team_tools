#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos.sh"         || exit 1
. "$( dirname "$0" )/_annotools_env.sh" || exit 2
port_sanity_check || exit 3

check_set_zmap_build_dir_from_arg "$@"
goto_build_root

echo "In ${PWD}"

# ZMap

zmap_tarball="${zmap_dist_dir}/zmap-*.tar.gz"
unpack_matching_tarball "${zmap_tarball}" "zmap-"
zmap_src="${unpacked_tarball}"
stage_config_make_install "${zmap_src}"

# Seqtools

seqtools_tarball="${zmap_dist_dir}/seqtools-*.tar.gz"
unpack_matching_tarball "${seqtools_tarball}" "seqtools-"
seqtools_src="${unpacked_tarball}"
stage_config_make_install "${seqtools_src}"

# XRemote - just copy it into place

xremote_rel_src_loc="ZMap/src/perl"
xremote_src_pattern="${zmap_build_dir}/${xremote_rel_src_loc}/X11-XRemote-*"
xremote_src_dir=$( echo ${xremote_src_pattern} ) # glob expansion
xremote_dest="${stage_dir}/${xremote_rel_src_loc}"

mkdir -v -p "${xremote_dest}"
echo "${xremote_src_dir} -> ${xremote_dest}"
cp -r "${xremote_src_dir}" "${xremote_dest}"

exit 0
