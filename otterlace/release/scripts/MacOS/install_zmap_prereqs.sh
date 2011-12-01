#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos.sh" || exit 1

# side-effects: sets $unpacked_tarball
#
unpack_matching_tarball() {
    local tarball expected_stem

    tarball_pattern="$1"
    expected_stem="$2"

    real_tarball=$( echo ${tarball_pattern} ) # do the glob expansion
    echo "Unpacking ${real_tarball}..."
    tar -xf "${real_tarball}"

    unpacked_tarball=$( echo ${expected_stem}* )
    echo "...to ${unpacked_tarball}"

    /usr/bin/true
}

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

extra_cflags="-arch i386 -mmacosx-version-min=10.5 -isysroot /Developer/SDKs/MacOSX10.5.sdk"
export MACOSX_DEPLOYMENT_TARGET=10.5
# Now we need access to some of our local tools
export PATH="${install_base}/bin:${PATH}"

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

(
    cd "$libaceconn_src"
    CFLAGS="${extra_cflags}" ./configure --prefix="${install_base}"
    make
    make install
)

exit 0
