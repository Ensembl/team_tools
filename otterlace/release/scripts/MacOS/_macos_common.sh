# Common functions and settings for inclusion in MacOS build scripts

macos_scripts="$( dirname "$0" )"
script_name="$( basename "$0" )"

# ensure we don't pick up any previous MacPorts installation
export PATH=/bin:/sbin:/usr/bin:/usr/sbin

export http_proxy=http://webcache.sanger.ac.uk:3128
export HTTPS_PROXY="${http_proxy}"
export ftp_proxy="${http_proxy}"

resources_path="Contents/Resources"

# EOF
