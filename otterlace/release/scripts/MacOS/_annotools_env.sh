# Settings for inclusion in annotools build scripts, with
#
#   . "$( dirname "$0" )/_annotools_env.sh" || exit 1

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

# EOF
