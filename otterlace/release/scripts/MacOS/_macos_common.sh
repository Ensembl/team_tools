# Common functions and settings for inclusion in MacOS build scripts

macos_scripts="$( dirname "$0" )"
script_name="$( basename "$0" )"

# based on this script being in $TT/otterlace/release/scripts/MacOS ...
etc_macos="${macos_scripts}/../../etc/MacOS"

# ensure we don't pick up any previous MacPorts installation
export PATH=/bin:/sbin:/usr/bin:/usr/sbin

export http_proxy=http://webcache.sanger.ac.uk:3128
export HTTPS_PROXY="${http_proxy}"
export ftp_proxy="${http_proxy}"

contents_macos_path="Contents/MacOS"
resources_path="Contents/Resources"

bail() {
    echo -e "$1" >&2
    exit 1
}

non_dist_dirs() {
    echo "$( grep '^-' "${etc_macos}/non_dist.list" | sed -e 's/^-//' )"
}

# EOF
