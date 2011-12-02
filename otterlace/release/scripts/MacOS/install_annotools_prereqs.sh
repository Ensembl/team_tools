#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos.sh"         || exit 1
. "$( dirname "$0" )/_annotools_env.sh" || exit 2

check_set_zmap_dist_dir_from_arg "$@"
goto_build_root

# ACEDB

acedb_tarball="${zmap_dist_dir}/ACEDB-*.src.tar.gz"
unpack_matching_tarball "${acedb_tarball}" "ACEDB-"
acedb_src="${unpacked_tarball}"

acedb_machine="DARWIN_MACPORTS_OTTER"
acedb_wmake_defs="${acedb_machine}_DEF"
acedb_wmake_defs_src="${etc_macos}/${acedb_wmake_defs}"
acedb_wmake_defs_dst="${acedb_src}/wmake/${acedb_wmake_defs}"

sed -e "s|OTT_REL_MACOS_INSTALL_BASE|${install_base}|"   \
    -e "s|OTT_REL_MACOS_EXTRA_CFLAGS|${extra_cflags}|"   \
   "${acedb_wmake_defs_src}" \
 > "${acedb_wmake_defs_dst}"

(
    cd "$acedb_src"
    ln -s wmake/makefile makefile
    export ACEDB_MACHINE="${acedb_machine}"
    make
)

# libAceConn

libaceconn_tarball="${zmap_dist_dir}/libAceConn-*.tar.gz"
unpack_matching_tarball "${libaceconn_tarball}" "libAceConn-"
libaceconn_src="${unpacked_tarball}"
config_make_install "${libaceconn_src}"

exit 0
