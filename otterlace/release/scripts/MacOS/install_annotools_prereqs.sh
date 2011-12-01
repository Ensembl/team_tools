#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos.sh"         || exit 1
. "$( dirname "$0" )/_annotools_env.sh" || exit 2

# FIXME: set via commandline
zmap_nfs="/net/netapp5a/vol/team119/zmap"
zmap_builds="${zmap_nfs}/BUILDS"

zmap_track="PRODUCTION_BUILDS"
zmap_ver="0-1-138"
zmap_distro=""			# null or e.g. ".lenny"

zmap_dir="${zmap_builds}/${zmap_track}/ZMap.${zmap_ver}.BUILD${zmap_distro}"
zmap_dist="${zmap_dir}/Dist"

echo "Fetching tarballs from ${zmap_dist}"

build_root="${install_base}/var/zmap_build"

mkdir -v -p "${build_root}"
cd "${build_root}"

# ACEDB

acedb_tarball="${zmap_dist}/ACEDB-*.src.tar.gz"
unpack_matching_tarball "${acedb_tarball}" "ACEDB-"
acedb_src="${unpacked_tarball}"

acedb_machine="DARWIN_MACPORTS_OTTER"
acedb_wmake_defs="${acedb_machine}_DEF"
acedb_wmake_defs_src="${etc_macos}/${acedb_wmake_defs}"
acedb_wmake_defs_dst="${acedb_src}/wmake/${acedb_wmake_defs}"

sed -e "s|ARM_INSTALL_BASE|${install_base}|"   \
    -e "s|ARM_EXTRA_CFLAGS|${extra_cflags}|"   \
   "${acedb_wmake_defs_src}" \
 > "${acedb_wmake_defs_dst}"

(
    cd "$acedb_src"
    ln -s wmake/makefile makefile
    export ACEDB_MACHINE="${acedb_machine}"
    make
)

# libAceConn

libaceconn_tarball="${zmap_dist}/libAceConn-*.tar.gz"
unpack_matching_tarball "${libaceconn_tarball}" "libAceConn-"
libaceconn_src="${unpacked_tarball}"
config_make_install "${libaceconn_src}"

exit 0
