# Functions for inclusion in sibling scripts, with
#
#   . "$( dirname "$0" )/_macos.sh" || exit 1

script_base="$( dirname "$0" )"

# assume we're being run in the top-level app dir, e.g. otterlace.app
app_base="${PWD}"

# based on this script being in $TT/otterlace/release/scripts/MacOS ...
etc_macos="${script_base}/../../etc/MacOS"

resource_path="Contents/Resources"
install_base="${app_base}/${resource_path}"

# ensure we don't pick up any previous MacPorts installation
export PATH=/bin:/sbin:/usr/bin:/usr/sbin

export http_proxy=http://webcache.sanger.ac.uk:3128
export HTTPS_PROXY="${http_proxy}"
export ftp_proxy="${http_proxy}"

# side-effects: sets $unpacked_tarball
#
unpack_matching_tarball() {
    local tarball expected_stem

    tarball_pattern="$1"
    expected_stem="$2"

    real_tarball=$( echo ${tarball_pattern} ) # do the glob expansion
    echo "Unpacking ${real_tarball}..."
    tar -xf "${real_tarball}"

    unpacked_tarball=$( echo ${PWD}/${expected_stem}* )
    echo "...to ${unpacked_tarball}"

    /usr/bin/true
}

# EOF
