#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos_in_app.sh"  || exit 1
. "$( dirname "$0" )/_annotools_env.sh" || exit 2
port_sanity_check || exit 3

check_set_zmap_build_dir_from_arg "$@"
goto_build_root

echo "In ${PWD}"

# Cleanup

echo "Removing previous zmap and seqtools builds, if any"
rm -rf zmap-* seqtools-*

annotools_binaries='belvu blixem blixemh dotter gffparser remotecontrol simpleclient simpletest xml_writer zmap'
if [ -d "${stage_prefix}/bin" ]; then
    for prog in ${annotools_binaries}; do
        rm -f "${stage_prefix}/bin/${prog}"
    done
fi

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

if [ -d "${xremote_src_dir}" ]; then
    mkdir -v -p "${xremote_dest}"
    echo "${xremote_src_dir} -> ${xremote_dest}"
    cp -r "${xremote_src_dir}" "${xremote_dest}"
else
    echo "XRemote not found in this ZMap build, so nothing more left to do."
fi

exit 0
