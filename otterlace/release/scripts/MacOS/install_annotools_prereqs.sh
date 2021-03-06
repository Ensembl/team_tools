#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos_in_app.sh"  || exit 1
. "$( dirname "$0" )/_annotools_env.sh" || exit 2
port_sanity_check || exit 3

check_set_zmap_build_dir_from_arg "$@"
goto_build_root

zmap_acedb_binaries='tace sgifaceserver xremote'

# Cleanup

echo "Removing previous ACEDB and libAceConn builds, if any"
rm -rf ACEDB-* libAceConn-*
if [ -d "${stage_prefix}/bin" ]; then
    for prog in ${zmap_acedb_binaries}; do
        rm -f "${stage_prefix}/bin/${prog}"
    done
fi

# ACEDB

acedb_tarball="${zmap_dist_dir}/ACEDB-*.tar.gz"
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
    cd "$acedb_src" &&
    ln -sf wmake/makefile makefile &&
    export ACEDB_MACHINE="${acedb_machine}" &&
    make &&
    install_binaries "bin.${acedb_machine}" "${stage_prefix}/bin" $zmap_acedb_binaries
)

if [ true ]; then

    echo "libAceConn is now part of ZMap, so skipping it here."

else

    # libAceConn

    libaceconn_tarball="${zmap_dist_dir}/libAceConn-*.tar.gz"
    unpack_matching_tarball "${libaceconn_tarball}" "libAceConn-"
    libaceconn_src="${unpacked_tarball}"
    config_make_install "${libaceconn_src}"

fi

exit 0
