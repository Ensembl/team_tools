# Settings for inclusion in annotools build scripts, with
#
#   . "$( dirname "$0" )/_annotools_env.sh" || exit 1

check_set_zmap_dist_dir_from_arg () {

    # sets global (but not exported) zmap_dist_dir
    # from supplied_arg
    zmap_dist_dir="$1"
    if [ ! -d "${zmap_dist_dir}" ]; then
	me="$( basename "$0" )"
	echo "Usage: $me <zmap_dist_dir>" >&2
	exit 3
    fi

    /usr/bin/true
}

# leaves build_root set, goes there and leaves us there
#
goto_build_root () {

    build_root="${install_base}/var/annotools_build"

    mkdir -v -p "${build_root}"
    cd "${build_root}"

    echo "Working in ${build_root}"

    /usr/bin/true
}
    
# These ensure we build for 10.5, i386.

extra_cflags="-arch i386 -mmacosx-version-min=10.5 -isysroot /Developer/SDKs/MacOSX10.5.sdk"
export MACOSX_DEPLOYMENT_TARGET=10.5

# Now we need access to some of our local tools
export PATH="${install_base}/bin:${PATH}"

config_make () {
    local src_dir
    src_dir="$1"

    # use a subshell to avoid permanent cd
    (
	cd "${src_dir}"

	CFLAGS="${extra_cflags}" ./configure --prefix="${install_base}"
	make

    )

    /usr/bin/true
}

config_make_install () {
    local src_dir
    src_dir="$1"

    # use a subshell to avoid permanent cd
    (
	cd "${src_dir}"

	config_make "${PWD}"
	make install
    )

    /usr/bin/true
}

install_binaries () {
    local src_dir dest
    src_dir="$1"
    shift

    dest="${install_base}/bin"

    for prog in "$@"; do
	cp -v "${src_dir}/${prog}" "${dest}/${prog}"
    done

    /usr/bin/true
}

# EOF
