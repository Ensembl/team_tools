# Functions for inclusion in scripts designed to run in app directory, with
#
#   . "$( dirname "$0" )/_macos_in_app.sh" || exit 1

. "$( dirname "${BASH_SOURCE[0]}" )/_macos_common.sh" || exit 9

# assume we're being run in the top-level app dir, e.g. otterlace.app
app_base="${PWD}"

install_base="${app_base}/${resources_path}"

# side-effects: sets $unpacked_tarball
#
unpack_matching_tarball() {
    local tarball_pattern real_tarball expected_stem

    tarball_pattern="$1"
    expected_stem="$2"

    real_tarball=$( echo ${tarball_pattern} ) # do the glob expansion
    echo "Unpacking ${real_tarball}..."
    tar -xf "${real_tarball}"

    unpacked_tarball=$( echo ${PWD}/${expected_stem}* )
    echo "...to ${unpacked_tarball}"

    /usr/bin/true
}

check_binary() {
    local binary binary_path
    binary="$1"

    binary_path="${install_base}/bin/${binary}"
    if [ -x "${binary_path}" ]; then
	/usr/bin/true
    else
	echo "Cannot find '${binary_path}'"
	/usr/bin/false
    fi
}

port_sanity_check() {

    if check_binary 'port'; then
	/usr/bin/true
    else
	echo "Check you're at the root of an app tree!"
	/usr/bin/false
    fi
}

perl_sanity_check() {
    local perl

    port_sanity_check || return
    check_binary 'perl'
}

# EOF
