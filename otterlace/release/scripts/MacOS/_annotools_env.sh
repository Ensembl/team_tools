# Settings for inclusion in annotools build scripts, with
#
#   . "$( dirname "$0" )/_annotools_env.sh" || exit 1

check_set_zmap_build_dir_from_arg () {

    # sets global (but not exported) zmap_build_dir
    # from supplied_arg, then also sets zmap_dist_dir
    zmap_build_dir="$1"
    if [ ! -d "${zmap_build_dir}" ]; then
	me="$( basename "$0" )"
	echo "Usage: $me <zmap_build_dir>" >&2
	exit 3
    fi

    zmap_dist_dir="${zmap_build_dir}/Dist"
    /usr/bin/true
}

# leaves build_root set, goes there and leaves us there
# also sets stage_root
goto_build_root () {

    build_root="${install_base}/var/annotools_build"
    stage_root="${build_root}/stage"

    mkdir -v -p "${build_root}"
    mkdir -v -p "${stage_root}"

    cd "${build_root}"
    echo "Working in ${build_root}"

    /usr/bin/true
}
    
# These ensure we build for 10.5, i386.

extra_cflags="-arch i386 -mmacosx-version-min=10.5 -isysroot /Developer/SDKs/MacOSX10.5.sdk"
export MACOSX_DEPLOYMENT_TARGET=10.5

# Now we need access to some of our local tools
export PATH="${install_base}/bin:${PATH}"

_do_config_make () {
    local src_dir prefix
    src_dir="$1"
    prefix="$2"

    # use a subshell to avoid permanent cd
    (
	cd "${src_dir}"

	CFLAGS="${extra_cflags}" ./configure --prefix="${prefix}"
	make

    )

    /usr/bin/true
}

_do_config_make_install () {
    local src_dir prefix
    src_dir="$1"
    prefix="$2"

    # use a subshell to avoid permanent cd
    (
	cd "${src_dir}"

	_do_config_make "${PWD}" "${prefix}"
	make install
    )

    /usr/bin/true
}

config_make () {
    local src_dir prefix
    src_dir="$1"

    prefix="${install_base}"
    _do_config_make "${src_dir}" "${prefix}"
}

config_make_install () {
    local src_dir prefix
    src_dir="$1"

    prefix="${install_base}"
    _do_config_make_install "${src_dir}" "${prefix}"
}

stage_config_make_install () {
    local src_dir prefix
    src_dir="$1"

    prefix="${stage_root}"
    _do_config_make_install "${src_dir}" "${prefix}"
}

install_binaries () {
    local src_dir dest_dir
    src_dir="$1"
    dest_dir="$2"
    shift
    shift

    mkdir -v -p "${dest_dir}"

    for prog in "$@"; do
	cp -v "${src_dir}/${prog}" "${dest_dir}/${prog}"
    done

    /usr/bin/true
}

# EOF
