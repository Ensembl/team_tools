#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos.sh"         || exit 1
. "$( dirname "$0" )/_annotools_env.sh" || exit 2

check_set_zmap_build_dir_from_arg "$@"
goto_build_root

echo "In ${PWD}"

# ZMap

zmap_tarball="${zmap_dist_dir}/zmap-*.tar.gz"
unpack_matching_tarball "${zmap_tarball}" "zmap-"
zmap_src="${unpacked_tarball}"
config_make "${zmap_src}"
install_binaries "${zmap_src}" zmap

# Seqtools

seqtools_tarball="${zmap_dist_dir}/seqtools-*.tar.gz"
unpack_matching_tarball "${seqtools_tarball}" "seqtools-"
seqtools_src="${unpacked_tarball}"
config_make_install "${seqtools_src}"
rm "${install_base}/bin/blixem"

# XRemote

xremote_src_pattern="${zmap_build_dir}/ZMap/src/perl/X11-XRemote-*"
xremote_src_dir=$( echo ${xremote_src_pattern} ) # glob expansion
cp -v -r "$xremote_src_dir" .
xremote_dir=$( echo ./X11-XRemote-* )
(
   cd "${xremote_dir}" &&
   echo "Making XRemote in ${PWD}" &&
   perl Makefile.PL \
       PREFIX="${install_base}" \
       --with-x-inc     "${install_base}/include" \
       --with-x-libs    "${install_base}/lib" \
       --with-zmap-inc  "${zmap_src}/include" \
       --with-zmap-libs "${zmap_src}/.libs" \
       --with-symbols \
       &&
   make &&
   make test &&
   make install &&
   /usr/bin/true
)

exit 0
